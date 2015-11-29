{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Boxen where

import Servant
import Control.Monad.Trans.Either
import Data.Functor.Identity
import Data.Time.LocalTime
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Hasql as H
import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Postgres

getBoxen :: DBConn -> EitherT ServantErr IO [BoxInfo]
getBoxen conn = do
    runTx conn (
      do (boxen :: [(Int,T.Text,T.Text,Maybe Int,Maybe LocalTime)])
             <- lift $ H.listEx $ [H.stmt|
                  SELECT id
                       , name
                       , mac
                       , boot_image
                       , boot_until
                  FROM boxen
                  ORDER BY name ASC
             |]
         boxInfos <- forM boxen (\(boxId,_,_,mImg,_)
             -> do mName <- case mImg of
                                Nothing -> return Nothing
                                Just imgId -> do
                                    (mname :: Maybe (Identity T.Text))
                                        <- lift $ H.maybeEx $ [H.stmt|
                                            SELECT name
                                            FROM images
                                            WHERE id = ?
                                        |] imgId
                                    return (mname >>= (Just . unwrapId))
                   (bFlags :: [(T.Text,Maybe T.Text)])
                        <- lift $ H.listEx $ [H.stmt|
                            SELECT key
                                 , value
                            FROM bootflags
                            WHERE box_id = ?
                            ORDER BY key ASC
                        |] boxId
                   (logs :: [(T.Text,LocalTime)])
                       <- lift $ H.listEx $ [H.stmt|
                           SELECT images.name
                                , boots.boot_time
                           FROM boots
                           INNER JOIN images ON (boots.image_id = images.id)
                           WHERE boots.box_id = ?
                           ORDER BY boots.boot_time DESC
                       |] boxId
                   return (mName,bFlags,logs)
             )
         return $ zip boxen boxInfos
     )
     (   right . map (\((_,name,macaddr,_,mUntil),(mName,bFlags,logs)) ->
            BoxInfo { boxName = name
                    , mac     = macaddr
                    , boot    = case (mName,mUntil) of
                                    (Just iName,Just til) ->
                                        Just $ BootSettings
                                            iName til (map (\(k,v) -> BootFlag k v) bFlags)
                                    _ -> Nothing
                    , bootlogs = map (\(n,t) -> BootInstance n t) logs
                    })
     )

newBox :: DBConn -> T.Text -> NewBox -> EitherT ServantErr IO ()
newBox conn n (NewBox m) = do
    runTx_ conn (do
            (res :: Maybe (Identity Int)) <- lift $ H.maybeEx $ [H.stmt|
                    SELECT id
                    FROM "boxen"
                    WHERE name = ?
                |] n
            when (res /= Nothing) (throwE
                 err400 { errBody = "a box with that name already exists" })
            (res' :: Maybe (Identity Int)) <- lift $ H.maybeEx $ [H.stmt|
                    SELECT id
                    FROM "boxen"
                    WHERE mac = ?
                |] (stripMac m)
            when (res' /= Nothing) (throwE
                 err400 { errBody = "a box with that MAC address already exists" })
            lift $ H.unitEx $ [H.stmt|
                    INSERT INTO "boxen"
                        (name,mac)
                    VALUES
                        (?,?)
                |] n (stripMac m)
        )

updateBox :: DBConn -> T.Text -> UpdateBox -> EitherT ServantErr IO ()
updateBox = undefined
--updateBox conn n (UpdateBox mImgName, mUntil, mFlags) = do
--    dbres <- liftIO $ do
--        H.session conn $ H.tx Nothing $ do
--            when (mImg /= Nothing) do
--                (mImgId :: Maybe (Identity Int)) <- H.maybeEx $ [H.stmt|
--                        SELECT id
--                        FROM "images"
--                        WHERE name = ?
--                    |]
--                
--                H.unitEx $ [H.stmt|
--                        UPDATE "boxen"
--                        SET 
--                            (name,mac)
--                        VALUES
--                            (?,?)
--                    |] n (stripMac m)
--    case dbres of
--        Left err -> left err500 { errBody = BS.pack $ show err }
--        Right _ -> right ()

deleteBox :: DBConn -> T.Text -> EitherT ServantErr IO ()
deleteBox  = undefined
