{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Boxen where

import Servant
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Time.LocalTime
import Control.Monad

import qualified Hasql as H
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

import Hydrazine.JSON
import Hydrazine.Postgres

getBoxen :: DBConn -> EitherT ServantErr IO [BoxInfo]
getBoxen conn = do
    dbres <- liftIO $ do
        H.session conn $ H.tx Nothing $ do
            (boxen :: [(Int,T.Text,T.Text,Maybe Int,Maybe LocalTime)]) <- H.listEx $
                [H.stmt|
                    SELECT id
                         , name
                         , mac
                         , boot_image
                         , boot_until
                    FROM boxen
                    ORDER BY name ASC
                |]
            boxInfos <- forM boxen (\(boxId,_,_,mImg,_) -> do
                mName <- case mImg of
                    Nothing -> return Nothing
                    Just imgId -> do
                        (name :: Maybe (Identity T.Text)) <- H.maybeEx $ [H.stmt|
                                SELECT name
                                FROM images
                                WHERE id = ?
                            |] imgId
                        let name' = case name of
                                        Just (Identity n) -> Just n
                                        Nothing -> Nothing
                        return name'
                bFlags <- do
                    (bFlags :: [(T.Text,Maybe T.Text)]) <- H.listEx $ [H.stmt|
                            SELECT key
                                 , value
                            FROM bootflags
                            WHERE box_id = ?
                            ORDER BY key ASC
                        |] boxId
                    return bFlags
                logs <- do
                    (logs :: [(T.Text,LocalTime)]) <- H.listEx $ [H.stmt|
                            SELECT images.name
                                 , boots.boot_time
                            FROM boots
                            INNER JOIN images ON (boots.image_id = images.id)
                            WHERE boots.box_id = ?
                            ORDER BY boots.boot_time DESC
                        |] boxId
                    return logs
                return (mName,bFlags,logs)
                )
            return $ zip boxen boxInfos
    case dbres of
        Left err -> left $ err500 { errBody = BS.pack $ show err }
        Right lst -> right $ map (\((_,name,macaddr,_,mUntil),(mName,bFlags,logs)) ->
            BoxInfo { boxName = name
                    , mac     = macaddr
                    , boot    = case (mName,mUntil) of
                                    (Just iName,Just til) ->
                                        Just $ BootSettings
                                            iName til (map (\(k,v) -> BootFlag k v) bFlags)
                                    _ -> Nothing
                    , bootlogs = map (\(n,t) -> BootInstance n t) logs
                    }) lst

newBox :: DBConn -> T.Text -> NewBox -> EitherT ServantErr IO ()
newBox  = undefined

updateBox :: DBConn -> T.Text -> UpdateBox -> EitherT ServantErr IO ()
updateBox  = undefined

deleteBox :: DBConn -> T.Text -> EitherT ServantErr IO ()
deleteBox  = undefined
