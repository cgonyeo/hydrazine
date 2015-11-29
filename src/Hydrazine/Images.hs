{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Images where

import Servant
import Control.Monad.Trans.Either
import Data.Maybe
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Time.LocalTime
import Control.Monad
import Control.Concurrent.MVar
import Network.Wai.Parse
import System.Directory
import System.FilePath
import Data.UUID
import Data.UUID.V4
import Data.Aeson
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Hasql as H
import qualified Data.Text as T

import Hydrazine.Config
import Hydrazine.JSON
import Hydrazine.Postgres

data Uploads = Uploads { uploadCounter :: Int
                       , activeUploads :: [ActiveUpload]
                       }

data ActiveUpload = ActiveUpload { uid         :: Int
                                 , name        :: T.Text
                                 , kernelPath  :: Maybe FilePath
                                 , uploadCPIOs :: [FilePath]
                                 } deriving(Eq)

data UploadErr = ErrNotInProgress
               | ErrAlreadyUploaded
               deriving(Eq)

getError :: UploadErr -> ServantErr
getError ErrNotInProgress   = err404 { errBody = "no such upload is in progress" }
getError ErrAlreadyUploaded = err400 { errBody = "kernel already uploaded" }

tmpKernelFile :: Int -> IO FilePath
tmpKernelFile num = do
        tmpdir <- getTemporaryDirectory
        return $ tmpdir </> (show num ++ "-kernel")

tmpCPIODir :: Int -> IO FilePath
tmpCPIODir uploadNum = do
        tmpdir <- getTemporaryDirectory
        let cpioDir = tmpdir </> (show uploadNum ++ "-cpio")
        exists <- doesDirectoryExist cpioDir
        when (not exists) $ createDirectory cpioDir
        return cpioDir

tmpCPIOFile :: Int -> Int -> IO FilePath
tmpCPIOFile uploadNum cpioNum = do
        cpioDir <- tmpCPIODir uploadNum
        return $ cpioDir </> show cpioNum

findUpload :: Int -> [ActiveUpload] -> Maybe ActiveUpload
findUpload i ((a@(ActiveUpload i' _ _ _)):as)
    | i == i' = Just a
    | otherwise = findUpload i as
findUpload _ [] = Nothing

getImages :: DBConn -> EitherT ServantErr IO [ImageInfo]
getImages conn = do
    runTx conn ( do
            (imgs :: [(Int,T.Text,LocalTime,T.Text)])
                <- lift $ H.listEx $ [H.stmt|
                    SELECT id
                         , name
                         , created
                         , kernel_path
                    FROM images
                    ORDER BY name ASC
                |]
            (cs :: [[T.Text]])
                <- forM imgs (\(imgId,_,_,_) -> do
                       cs <- lift $ H.listEx $ [H.stmt|
                            SELECT cpio_path
                            FROM cpios
                            WHERE image_id = ?
                            ORDER BY ordering ASC
                        |] imgId
                       return $ map unwrapId cs
                )
            (fs :: [[(T.Text,Maybe T.Text)]])
                <- forM imgs (\(imgId,_,_,_) ->
                    lift $ H.listEx $ [H.stmt|
                            SELECT key
                                 , value
                            FROM defaultbootflags
                            WHERE image_id = ?
                            ORDER BY key ASC
                        |] imgId
                )
            return $ zip3 imgs cs fs
        )
        (
            right . map (\((_,n,c,k),cs,fs) ->
                ImageInfo n c k cs (map (\(key,v) -> BootFlag key v) fs))
        )

newUpload :: DBConn -> MVar Uploads -> NewImage -> EitherT ServantErr IO UploadID
newUpload conn mups (NewImage n) =
    runTx conn ( do
            (res :: Maybe (Identity Int))
                <- lift $ H.maybeEx $ [H.stmt|
                        SELECT id
                        FROM "images"
                        WHERE name = ?
                    |] n
            when (isJust res) $
                throwE err400 { errBody = "an image with that name already exists" }
        )
        (\_ -> do
            i <- liftIO $ modifyMVar mups (\(Uploads c us) ->
                        return ((Uploads (c+1) ((ActiveUpload c n Nothing []):us)),c))
            right $ UploadID i
        )

uploadKernel :: MVar Uploads -> Int -> [File FilePath] -> EitherT ServantErr IO ()
uploadKernel _ _ []      = left $ err400 { errBody = "no file uploaded" }
uploadKernel _ _ (_:_:_) = left $ err400 { errBody = "too many files uploaded" }
uploadKernel mups i [(_,fileinfo)] = do
    (mname,merr) <- liftIO $ modifyMVar mups (\ups@(Uploads c as) -> do
        case findUpload i as of
            Nothing -> return (ups,(Nothing,Just ErrNotInProgress))
            Just (ActiveUpload _ _ (Just _) _) ->
                    return (ups,(Nothing,Just ErrAlreadyUploaded))
            Just (ActiveUpload _ _ Nothing _) -> do
                    kFile <- tmpKernelFile i
                    let newActiveUploads = 
                         map (\a -> if uid a == i
                                         then a { kernelPath = Just kFile }
                                         else a) as
                    return ( (Uploads c newActiveUploads)
                           , (Just $ kFile,Nothing)
                           )
            )
    case merr of
        Just err -> left $ getError err
        Nothing -> do liftIO $ do
                            let kFile = fromJust mname
                            renameFile (fileContent fileinfo) kFile
                      right ()

uploadCPIO :: MVar Uploads -> Int -> [File FilePath] -> EitherT ServantErr IO ()
uploadCPIO _ _ [] = left $ err400 { errBody = "no file uploaded" }
uploadCPIO mups i files = do
    (newfiles,merrs) <- unzip <$> liftIO (forM files (\(_,fileinfo) -> do
            modifyMVar mups (\ups@(Uploads c as) -> do
                    case findUpload i as of
                        Nothing -> return (ups,(Nothing,Just ErrNotInProgress))
                        Just (ActiveUpload _ _ _ cs) -> do
                            cFile <- tmpCPIOFile i (length cs)
                            let as' = map (\a -> if uid a == i
                                                    then a { uploadCPIOs = cs ++ [cFile] }
                                                    else a) as
                            return ( (Uploads c as')
                                   , (Just (cFile,fileContent fileinfo),Nothing)
                                   )
                )
        ))
    let merrs' = filter (/= Nothing) merrs
    case merrs' of
        (Just err:_) -> left $ getError err
        _ -> do liftIO $ forM_ newfiles (\(Just (new,old)) -> renameFile old new)
                right ()

imageAlreadyExists :: DBConn -> T.Text -> EitherT ServantErr IO (Maybe ServantErr)
imageAlreadyExists conn n =
    runTx conn ( do
            (res :: Maybe (Identity Int))
                <- lift $ H.maybeEx $ [H.stmt|
                    SELECT id
                    FROM images
                    WHERE name = ?
                |] n
            return res
        )
        (\res ->
            case res of
                Nothing -> right $ Nothing
                Just _ -> right $ Just err400 { errBody = "image with that name already exists" }
        )

completeUpload :: Config -> DBConn -> MVar Uploads -> Int -> EitherT ServantErr IO UploadResults
completeUpload (Config fd) conn mups i = do
    res <- liftIO $ modifyMVar mups (\(Uploads c as) -> do
                    case findUpload i as of
                        Just u -> return (Uploads c (filter (/= u) as),(Right u))
                        Nothing -> return (Uploads c as,(Left ErrNotInProgress))
                )
    case res of
        Left err -> left $ getError err
        Right (ActiveUpload _ _ Nothing _) -> right $ UploadResults False "kernel was not uploaded"
        Right (ActiveUpload _ _ _ []) -> right $ UploadResults False "cpio was not uploaded"
        Right (ActiveUpload _ n mkFile cs) -> do
            merr <- imageAlreadyExists conn n
            case merr of
                Just err -> left err
                Nothing -> do
                    (finalKFile,finalCFiles) <- liftIO $ do
                        uuid <- toString <$> nextRandom
                        let finalKFile = fd </> (uuid ++ ".vmlinuz")
                        renameFile (fromJust mkFile) finalKFile
                        finalCFiles <- forM (zip cs ([0..] :: [Int])) (\(cFile,num) -> do
                            let finalCFile = fd </> (uuid ++ "." ++ show num ++ "." ++ ".cpio")
                            renameFile cFile finalCFile
                            return finalCFile
                            )
                        return (finalKFile,finalCFiles)
                    dbres <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            (Identity imgId :: Identity Int) <- H.singleEx $ [H.stmt|
                                    INSERT INTO "images"
                                        (name,kernel_path,created)
                                    VALUES
                                        (?,?,now())
                                    RETURNING id
                                |] n finalKFile
                            forM_ (zip finalCFiles ([0..] :: [Int])) (\(c,num) -> do
                                    H.unitEx $ [H.stmt|
                                            INSERT INTO "cpios"
                                                (image_id,ordering,cpio_path)
                                            VALUES
                                                (?,?,?)
                                        |] imgId num c
                               )
                    case dbres of
                        Left err -> left $ err500 { errBody = encode $ UploadResults False (T.pack $ show err) }
                        Right () -> right $ UploadResults True ""

deleteImage :: DBConn -> name -> EitherT ServantErr IO ()
deleteImage = undefined
