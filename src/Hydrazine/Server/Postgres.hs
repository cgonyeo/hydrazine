{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hydrazine.Server.Postgres where

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Data.Maybe
import Control.Monad.Trans.Either
import Servant
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Data.ByteString.Lazy.Char8 as BS

type DBConn = H.Pool HP.Postgres

postgresSettings :: HP.Settings
postgresSettings = HP.ParamSettings "localhost"      -- Host
                                    5432             -- Port
                                    "hacs"           -- User
                                    "treestheyareus" -- Password
                                    "hacs"      -- Database

-- fromJust is used because the following will never be Nothing.
-- 6 and 30 both fall within valid bounds.
poolSettings :: H.PoolSettings
poolSettings = fromJust $ H.poolSettings 6 30

newDBConn :: IO DBConn
newDBConn = H.acquirePool postgresSettings poolSettings

runTx :: DBConn
      -> (forall s. ExceptT ServantErr (H.Tx HP.Postgres s) b)
      -> (b -> EitherT ServantErr IO c)
      -> EitherT ServantErr IO c
runTx conn tx handleResult = do
    dbres <- liftIO $ H.session conn $ H.tx Nothing $ runExceptT $ tx
    case dbres of
        Left err -> left err500 { errBody = BS.pack $ show err }
        Right (Left err) -> left err
        Right (Right res) -> handleResult res

runTx_ :: DBConn
       -> (forall s. ExceptT ServantErr (H.Tx HP.Postgres s) b)
       -> EitherT ServantErr IO ()
runTx_ conn tx = runTx conn tx (\_ -> right ())

unwrapId :: Identity a -> a
unwrapId (Identity x) = x
