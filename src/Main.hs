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

import Hydrazine.API
import Hydrazine.Config
import Hydrazine.Postgres
import Hydrazine.Images

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
