{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Server.Boxen where

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
import Hydrazine.Server.Postgres

getBoxen :: DBConn -> EitherT ServantErr IO [BoxInfo]
getBoxen conn = do
    runTx conn (
      do (boxen :: [(Int,T.Text,T.Text,Maybe Int,Maybe LocalTime,Bool)])
             <- lift $ H.listEx $ [H.stmt|
                  SELECT id
                       , name
                       , mac
                       , boot_image
                       , boot_until
                       , boot_forever
                  FROM boxen
                  ORDER BY name ASC
             |]
         boxInfos <- forM boxen (\(boxId,_,_,mImg,_,_)
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
                           SELECT image_name
                                , boot_time
                           FROM boots
                           WHERE boots.box_id = ?
                           ORDER BY boots.boot_time DESC
                       |] boxId
                   return (mName,bFlags,logs)
             )
         return $ zip boxen boxInfos
     )
     (   right . map (\((_,name,macaddr,_,mUntil,bForever),(mName,bFlags,logs)) ->
            BoxInfo { boxName = name
                    , mac     = formatMac macaddr
                    , boot    = let fs = map (\(k,v) -> BootFlag k v) bFlags
                                in case mName of
                                       Nothing -> Nothing
                                       Just iName -> Just $ BootSettings
                                            iName (BootUntil bForever mUntil) fs
                    , bootlogs = map (\(n,t) -> BootInstance n t) logs
                    })
     )

getBox :: DBConn -> T.Text -> EitherT ServantErr IO BoxInfo
getBox conn n = do
    runTx conn (
      do (mBox :: Maybe (Int,T.Text,T.Text,Maybe Int,Maybe LocalTime,Bool))
             <- lift $ H.maybeEx $ [H.stmt|
                  SELECT id
                       , name
                       , mac
                       , boot_image
                       , boot_until
                       , boot_forever
                  FROM boxen
                  WHERE name = ?
                  ORDER BY name ASC
             |] n
         when (isNothing mBox) $
            throwE err404 { errBody = "machine not found" }

         let box@(boxId,_,_,mImg,_,_) = fromJust mBox

         mName <- case mImg of
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
                 SELECT image_name
                      , boot_time
                 FROM boots
                 WHERE boots.box_id = ?
                 ORDER BY boots.boot_time DESC
             |] boxId
         return (box,mName,bFlags,logs)
     )
     (  
        \((_,_,macaddr,_,mUntil,bForever),mName,bFlags,logs) ->
            right $
            BoxInfo { boxName = n
                    , mac     = formatMac macaddr
                    , boot    = mName >>= (\iName -> Just $ BootSettings
                                                iName (BootUntil bForever mUntil) (map (\(k,v) -> BootFlag k v) bFlags))
                    , bootlogs = map (\(i,t) -> BootInstance i t) logs
                    }
     )

newBox :: DBConn -> T.Text -> NewBox -> EitherT ServantErr IO EmptyValue
newBox conn n (NewBox m) = do
    runTx conn (do
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
        ) (returnEmptyValue)

updateBox :: DBConn -> T.Text -> UpdateBox -> EitherT ServantErr IO EmptyValue
updateBox conn name (UpdateBox img til fs) =
    runTx conn (do
            (boxId :: Maybe (Identity Int))
                <- lift $ H.maybeEx $ [H.stmt|
                        SELECT id
                        FROM "boxen"
                        WHERE name = ?
                    |] name
            when (isNothing boxId) $
                throwE err400 { errBody = "a box with that name doesn't exist" }

            when (isJust img) $
                if (fromJust img) == ""
                    then lift $ H.unitEx $ [H.stmt|
                                UPDATE "boxen"
                                SET boot_image = NULL
                                WHERE name = ?
                            |] name
                    else do
                        (imgId :: Maybe (Identity Int))
                            <- lift $ H.maybeEx $ [H.stmt|
                                    SELECT id
                                    FROM "images"
                                    WHERE name = ?
                                |] img
                        when (isNothing imgId) $
                            throwE err400 { errBody = "image doesn't exist" }
                        lift $ H.unitEx $ [H.stmt|
                                UPDATE "boxen"
                                SET boot_image = ?
                                WHERE name = ?
                            |] (unwrapId $ fromJust imgId) name
                        when (isNothing fs) $ do
                            lift $ H.unitEx $ [H.stmt|
                                    DELETE FROM "bootflags"
                                    WHERE box_id = ?
                                |] (unwrapId $ fromJust boxId)
                            (bfs :: [(T.Text,Maybe T.Text)])
                                <- lift $ H.listEx $ [H.stmt|
                                    SELECT key
                                         , value
                                    FROM "defaultbootflags"
                                    WHERE image_id = ?
                                |] (unwrapId $ fromJust imgId)
                            forM_ bfs (\(key,val) ->
                                lift $ H.unitEx $ [H.stmt|
                                        INSERT INTO "bootflags"
                                            (box_id,key,value)
                                        VALUES
                                            (?,?,?)
                                    |] (unwrapId $ fromJust boxId) key val
                                )

            when (isJust til) $ 
                let (BootUntil bForever mUntil) = fromJust til
                in lift $ H.unitEx $ [H.stmt|
                        UPDATE "boxen"
                        SET boot_forever = ?
                          , boot_until = ?
                        WHERE name = ?
                    |] bForever mUntil name

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
        ) (returnEmptyValue)

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
                    WHERE id = ?
                |] boxId
        )
