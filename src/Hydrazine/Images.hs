{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Images where

import Servant
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Time.LocalTime
import Control.Monad

import qualified Hasql as H
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

import Hydrazine.JSON
import Hydrazine.Postgres

getImages :: DBConn -> EitherT ServantErr IO [ImageInfo]
getImages conn = do
    dbres <- liftIO $ do
        H.session conn $ H.tx Nothing $ do
            (imgs :: [(Int,T.Text,LocalTime,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT id
                         , name
                         , created
                         , kernel_path
                    FROM images
                    WHERE active = TRUE
                    ORDER BY name ASC
                |]
            cs <- forM imgs (\(imgId,_,_,_) -> do
                    (cs :: [(Identity T.Text)]) <- H.listEx $ [H.stmt|
                            SELECT cpio_path
                            FROM cpios
                            WHERE image_id = ?
                            ORDER BY ordering ASC
                        |] imgId
                    let cs' = map (\(Identity x) -> x) cs
                    return cs'
                )
            return $ zip imgs cs
    case dbres of
        Left err -> left $ err500 { errBody = BS.pack $ show err }
        Right lst -> right $ map (\((_,n,c,k),cs) ->
                                ImageInfo n c k cs) lst

newImage :: DBConn -> T.Text -> NewImage -> EitherT ServantErr IO ()
newImage  = undefined

updateImage :: DBConn -> T.Text -> NewImage -> EitherT ServantErr IO ()
updateImage  = undefined

deleteImage :: DBConn -> name -> EitherT ServantErr IO ()
deleteImage = undefined
