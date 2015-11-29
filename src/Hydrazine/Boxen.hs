{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Boxen where

import Servant
import Control.Monad.Trans.Either
import Data.Functor.Identity
import Data.Time.LocalTime
import Control.Monad
import Data.Maybe
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
updateBox conn name (UpdateBox img til fs) =
    runTx_ conn (do
            (boxId :: Maybe (Identity Int))
                <- lift $ H.maybeEx $ [H.stmt|
                        SELECT id
                        FROM "boxen"
                        WHERE name = ?
                    |] name
            when (isNothing boxId) $
                throwE err400 { errBody = "a box with that name doesn't exist" }

            when (isJust img) $ do
                (imgId :: Maybe (Identity Int))
                    <- lift $ H.maybeEx $ [H.stmt|
                            SELECT id
                            FROM "images"
                            WHERE name = ?
                        |] img
                when (isNothing imgId) $
                    throwE err400 { errBody = "an image with that name doesn't exist" }
                lift $ H.unitEx $ [H.stmt|
                        UPDATE "boxen"
                        SET boot_image = ?
                        WHERE name = ?
                    |] (unwrapId $ fromJust imgId) name

            when (isJust til) $ 
                lift $ H.unitEx $ [H.stmt|
                        UPDATE "boxen"
                        SET boot_until = ?
                        WHERE name = ?
                    |] (fromJust til) name

            when (isJust fs) $ do
                lift $ H.unitEx $ [H.stmt|
                        DELETE FROM "bootflags"
                        WHERE box_id = ?
                    |] (unwrapId $ fromJust boxId)
                forM_ (fromJust fs) (\(BootFlag key val) -> 
                        lift $ H.unitEx $ [H.stmt|
                                INSERT INTO "bootflags"
                                    (box_id,key,value)
                                VALUES
                                    (?,?,?)
                            |] (unwrapId $ fromJust boxId) key val
                    )
        )

deleteBox :: DBConn -> T.Text -> EitherT ServantErr IO ()
deleteBox conn name = 
    runTx_ conn (do
            (mBoxId :: Maybe (Identity Int))
                <- lift $ H.maybeEx $ [H.stmt|
                        SELECT id
                        FROM "boxen"
                        WHERE name = ?
                    |] name
            when (isNothing mBoxId) $
                throwE err400 { errBody = "a box with that name doesn't exist" }

            let boxId = unwrapId $ fromJust mBoxId

            lift $ H.unitEx $ [H.stmt|
                    DELETE FROM "boots"
                    WHERE box_id = ?
                |] boxId

            lift $ H.unitEx $ [H.stmt|
                    DELETE FROM "bootflags"
                    WHERE box_id = ?
                |] boxId

            lift $ H.unitEx $ [H.stmt|
                    DELETE FROM "boxen"
                    WHERE box_id = ?
                |] boxId
        )
