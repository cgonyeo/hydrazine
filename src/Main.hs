{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Postgres
import Hydrazine.Boot
import Hydrazine.Images
import Hydrazine.Boxen

hydrazineAPI :: Proxy HydrazineAPI
hydrazineAPI = Proxy

app :: DBConn -> Application
app conn = serve hydrazineAPI (server conn)

main :: IO ()
main = do conn <- newDBConn
          run 8081 $ logStdout (app conn)

type HydrazineAPI =
        "v1" :> "boot" :> Capture "mac" T.Text :> Get '[JSON] BootInfo
   :<|> "images" :> Get '[JSON] [ImageInfo]
   :<|> "images" :> Capture "name" T.Text :> ReqBody '[JSON] NewImage :> Post '[] ()
   :<|> "images" :> Capture "name" T.Text :> ReqBody '[JSON] NewImage :> Put '[] ()
   :<|> "images" :> Capture "name" T.Text :> Delete '[] ()
   :<|> "machines" :> Get '[JSON] [BoxInfo]
   :<|> "machines" :> Capture "name" T.Text :> ReqBody '[JSON] NewBox :> Post '[] ()
   :<|> "machines" :> Capture "name" T.Text :> ReqBody '[JSON] UpdateBox :> Put '[] ()
   :<|> "machines" :> Capture "name" T.Text :> Delete '[] ()

server :: DBConn -> Server HydrazineAPI
server conn = (getBootInfo conn)
    :<|> (getImages conn)
    :<|> (newImage conn)
    :<|> (updateImage conn)
    :<|> (deleteImage conn)
    :<|> (getBoxen conn)
    :<|> (newBox conn)
    :<|> (updateBox conn)
    :<|> (deleteBox conn)
