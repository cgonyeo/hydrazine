{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Server.API where

import Servant

import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Server.FileBackend

hydrazineAPI :: Proxy HydrazineAPI
hydrazineAPI = Proxy

type HydrazineAPI =
        "v1" :> "boot" :> Capture "mac" T.Text :> Get '[JSON] BootInfo
   :<|> "images" :> Get '[JSON] [ImageInfo]
   :<|> "images" :> "upload" :> "new" :> ReqBody '[JSON] NewImage :> Post '[JSON] UploadID
   :<|> "images" :> "upload" :> "kernel" :> Capture "uploadID" Int :> FilesTmp :> Post '[] ()
   :<|> "images" :> "upload" :> "cpio" :> Capture "uploadID" Int :> FilesTmp :> Post '[] ()
   :<|> "images" :> "upload" :> "complete" :> Capture "uploadID" Int :> Post '[JSON] ()
   :<|> "images" :> Capture "name" T.Text :> Delete '[] ()
   :<|> "machines" :> Get '[JSON] [BoxInfo]
   :<|> "machines" :> Capture "name" T.Text :> ReqBody '[JSON] NewBox :> Post '[JSON] EmptyValue
   :<|> "machines" :> Capture "name" T.Text :> ReqBody '[JSON] UpdateBox :> Put '[JSON] EmptyValue
   :<|> "machines" :> Capture "name" T.Text :> Delete '[] ()
   :<|> "files" :> Raw
