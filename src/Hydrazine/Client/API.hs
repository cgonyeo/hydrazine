{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Client.API where

import Servant
import Servant.Client
import Control.Monad.Trans.Either
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Client (Response)
import Network.HTTP.Types.Method (Method)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Types.Header as H (Header)

import Hydrazine.Server.API
import Hydrazine.JSON

getBootInfo :: T.Text -> EitherT ServantError IO BootInfo

getImages :: EitherT ServantError IO [ImageInfo]

newUpload :: NewImage -> EitherT ServantError IO UploadID

uploadKernel :: Int -> _ -> EitherT ServantError IO ()

uploadCPIO :: Int -> _ -> EitherT ServantError IO ()

completeUpload :: Int -> EitherT ServantError IO ()

deleteImage :: T.Text -> EitherT ServantError IO ()

getMachines :: EitherT ServantError IO [BoxInfo]

getMachine :: T.Text -> EitherT ServantError IO BoxInfo

newMachine :: T.Text -> NewBox -> EitherT ServantError IO EmptyValue

updateMachine :: T.Text -> UpdateBox -> EitherT ServantError IO EmptyValue

deleteMachine :: T.Text -> EitherT ServantError IO ()

staticFiles :: Method -> EitherT ServantError IO (Int, BS.ByteString, MediaType, [H.Header], Response BS.ByteString)

getBootInfo
 :<|> getImages
 :<|> newUpload
 :<|> uploadKernel
 :<|> uploadCPIO
 :<|> completeUpload
 :<|> deleteImage
 :<|> getMachines
 :<|> getMachine
 :<|> newMachine
 :<|> updateMachine
 :<|> deleteMachine
 :<|> staticFiles
    = client hydrazineAPI (BaseUrl Http "localhost" 8081)
