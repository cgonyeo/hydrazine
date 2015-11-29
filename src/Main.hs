{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Servant
import Control.Concurrent.MVar
import Options.Applicative

import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Postgres
import Hydrazine.Boot
import Hydrazine.Images
import Hydrazine.Boxen
import Hydrazine.FileBackend
import Hydrazine.Config

main :: IO ()
main = execParser opts >>= initAndRun
    where opts = info (helper <*> configParser)
                    ( fullDesc
                   <> header "hydrazine - a server for pixiecore"
                    )

initAndRun :: Config -> IO ()
initAndRun conf = do
        conn <- newDBConn
        mups <- newMVar (Uploads 0 [])
        run 8081 $ logStdout $ gzip def (app conf conn mups)

app :: Config -> DBConn -> MVar Uploads -> Application
app conf conn mups = serve hydrazineAPI (server conf conn mups)

hydrazineAPI :: Proxy HydrazineAPI
hydrazineAPI = Proxy

type HydrazineAPI =
        "v1" :> "boot" :> Capture "mac" T.Text :> Get '[JSON] BootInfo
   :<|> "images" :> Get '[JSON] [ImageInfo]
   :<|> "images" :> "upload" :> "new" :> ReqBody '[JSON] NewImage :> Post '[JSON] UploadID
   :<|> "images" :> "upload" :> "kernel" :> Capture "uploadID" Int :> FilesTmp :> Post '[] ()
   :<|> "images" :> "upload" :> "cpio" :> Capture "uploadID" Int :> FilesTmp :> Post '[] ()
   :<|> "images" :> "upload" :> "complete" :> Capture "uploadID" Int :> Post '[JSON] UploadResults
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
