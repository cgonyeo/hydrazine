{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Boot where

import Servant
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Aeson

import qualified Hasql as H
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

import Hydrazine.JSON
import Hydrazine.Postgres

getBootInfo :: DBConn -> T.Text -> EitherT ServantErr IO BootInfo
getBootInfo conn macaddr = do
    dbres <- liftIO $ do
        H.session conn $ H.tx Nothing $ do
            (mimage :: Maybe (Int,Int,T.Text)) <- H.maybeEx $ [H.stmt|
                    SELECT images.id
                         , boxen.id
                         , images.kernel_path
                    FROM images
                    INNER JOIN boxen ON (boxen.boot_image = images.id)
                    WHERE boxen.mac = ? AND images.active = TRUE
                |] (stripMac macaddr)
            case mimage of
                Nothing -> return Nothing
                Just (imgId,boxId,k) -> do
                    (cs :: [Identity T.Text]) <- H.listEx $ [H.stmt|
                            SELECT cpios.cpio_path
                            FROM cpios
                            INNER JOIN images ON (cpios.image_id = images.id)
                            WHERE images.id = ?
                            ORDER BY ordering ASC
                        |] imgId
                    (fs :: [(T.Text,Maybe T.Text)]) <- H.listEx $ [H.stmt|
                            SELECT key
                                 , value
                            FROM bootflags
                            WHERE box_id = ?
                        |] boxId
                    return $ Just (k,(map (\(Identity x) -> x) cs),fs)
    case dbres of
        Left err -> left $ err500 { errBody = BS.pack $ show err }
        Right Nothing -> left $ err404 { errBody = 
                                    "that MAC address is not in the system" }
        Right (Just (k,cs,fs)) ->
            right $ BootInfo { kernel = k
                             , initrd = cs
                             , cmdline = toJSONObject fs
                             }

toJSONObject :: [(T.Text,Maybe T.Text)] -> Value
toJSONObject = object . map (\(key,mval) ->
                                case mval of
                                    Just val -> key .= val
                                    Nothing  -> key .= True)
