{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hydrazine.FileBackend where

import qualified Data.ByteString.Lazy as BS

import Control.Monad.Trans.Resource
import Network.Wai.Parse
import Servant
import Servant.Client
import Control.Monad.Trans.Either

data Mem

data Tmp

class KnownBackend b where
    type Storage b :: *

    withBackend :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackend Mem where
    type Storage Mem = BS.ByteString

    withBackend Proxy f = f lbsBackEnd

instance KnownBackend Tmp where
    type Storage Tmp = FilePath

    withBackend Proxy f = runResourceT . withInternalState $ \s ->
        f (tempFileBackEnd s)

-- * Files combinator, to get all of the uploaded files

data Files b

instance (KnownBackend b, HasServer api) => HasServer (Files b :> api) where
    type ServerT (Files b :> api) m =
        [File (Storage b)] -> ServerT api m

    route Proxy subserver req respond = withBackend pb $ \b -> do
        (_, files) <- parseRequestBody b req
        route (Proxy :: Proxy api) (subserver files) req respond

        where pb = Proxy :: Proxy b

type FilesMem = Files Mem
type FilesTmp = Files Tmp

instance HasClient (FilesTmp :> Post '[] a) where
        type Client (FilesTmp :> Post '[] a) = EitherT ServantError IO a

        clientWithRoute Proxy req baseurl = undefined
