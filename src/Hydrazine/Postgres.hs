{-# LANGUAGE OverloadedStrings #-}
module Hydrazine.Postgres where

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Data.Maybe

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
