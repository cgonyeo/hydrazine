{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Server.Boot where

import Servant
import Control.Monad.Trans.Either
import Data.Functor.Identity
import Data.Aeson
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Hasql as H
import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Server.Postgres

getBootInfo :: DBConn -> T.Text -> EitherT ServantErr IO BootInfo
getBootInfo conn macaddr =
    runTx conn ( do
            (mimage :: Maybe (Int,Int,T.Text))
                <- lift $ H.maybeEx $ [H.stmt|
                    SELECT images.id
                         , boxen.id
                         , images.kernel_path
                    FROM images
                    INNER JOIN boxen ON (boxen.boot_image = images.id)
                    WHERE boxen.mac = ? AND (boxen.boot_forever = TRUE OR boxen.boot_until > now())
                |] (stripMac macaddr)
            when (isNothing mimage) $
                throwE err404 { errBody = "that MAC address is not in the system" }

            let (Just (imgId,boxId,k)) = mimage

            (cs :: [Identity T.Text])
                <- lift $ H.listEx $ [H.stmt|
                    SELECT cpios.cpio_path
                    FROM cpios
                    INNER JOIN images ON (cpios.image_id = images.id)
                    WHERE images.id = ?
                    ORDER BY ordering ASC
                |] imgId
            (fs :: [(T.Text,Maybe T.Text)])
                <- lift $ H.listEx $ [H.stmt|
                    SELECT key
                         , value
                    FROM bootflags
                    WHERE box_id = ?
                |] boxId
            return $ (k,(map (unwrapId) cs),fs)
        )
        (\(k,cs,fs) ->
            right $ BootInfo { kernel = k
                             , initrd = cs
                             , cmdline = toJSONObject fs
                             }
        )

toJSONObject :: [(T.Text,Maybe T.Text)] -> Value
toJSONObject = object . map (\(key,mval) ->
                                case mval of
                                    Just val -> key .= val
                                    Nothing  -> key .= True)
