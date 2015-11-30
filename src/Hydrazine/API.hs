{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.API where

import Servant
import Control.Concurrent.MVar

import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Postgres
import Hydrazine.Boot
import Hydrazine.Images
import Hydrazine.Boxen
import Hydrazine.FileBackend
import Hydrazine.Config

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
   :<|> "machines" :> Capture "name" T.Text :> ReqBody '[JSON] NewBox :> Post '[] ()
   :<|> "machines" :> Capture "name" T.Text :> ReqBody '[JSON] UpdateBox :> Put '[] ()
   :<|> "machines" :> Capture "name" T.Text :> Delete '[] ()
   :<|> "files" :> Raw

server :: Config -> DBConn -> MVar Uploads -> Server HydrazineAPI
server conf conn mups = (getBootInfo    conn)
                   :<|> (getImages      conn)
                   :<|> (newUpload      conn mups)
                   :<|> (uploadKernel   mups)
                   :<|> (uploadCPIO     mups)
                   :<|> (completeUpload conf conn mups)
                   :<|> (deleteImage    conn)
                   :<|> (getBoxen       conn)
                   :<|> (newBox         conn)
                   :<|> (updateBox      conn)
                   :<|> (deleteBox      conn)
                   :<|> (serveDirectory ".")
