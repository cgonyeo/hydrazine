{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.JSON where

import Data.Aeson
import Control.Applicative
import Data.Time.LocalTime
import Data.Char

import qualified Data.Text as T

stripMac :: T.Text -> T.Text
stripMac = T.filter (isAlphaNum) . T.toUpper

formatMac :: T.Text -> T.Text
formatMac m 
  | T.length m < 2 = m
  | otherwise      = T.take 2 m `T.append` ":" `T.append` formatMac (T.drop 2 m)

data BootInfo = BootInfo { kernel  :: T.Text
                         , initrd  :: [T.Text]
                         , cmdline :: Value
                         } deriving(Eq,Show)

instance ToJSON BootInfo where
    toJSON (BootInfo k i c) = 
        object [ "kernel"  .= k
               , "initrd"  .= i
               , "cmdline" .= c
               ]
instance FromJSON BootInfo where
    parseJSON (Object v) = BootInfo
                             <$> v .: "kernel"
                             <*> v .: "initrd"
                             <*> v .: "cmdline"
    parseJSON _          = empty

data ImageInfo = ImageInfo { imgName      :: T.Text
                           , created      :: LocalTime
                           , imgKernel    :: T.Text
                           , cpios        :: [T.Text]
                           , defaultflags :: [BootFlag]
                           } deriving(Eq,Show)

instance ToJSON ImageInfo where
    toJSON (ImageInfo n c k cs fs) =
        object [ "name"          .= n
               , "created"       .= c
               , "kernel"        .= k
               , "cpios"         .= cs
               , "default_flags" .= fs
               ]
instance FromJSON ImageInfo where
    parseJSON (Object v) = ImageInfo
                             <$> v .: "name"
                             <*> v .: "created"
                             <*> v .: "kernel"
                             <*> v .: "cpios"
                             <*> v .: "default_flags"
    parseJSON _          = empty

data BoxInfo = BoxInfo { boxName  :: T.Text
                       , mac      :: T.Text
                       , boot     :: Maybe BootSettings
                       , bootlogs :: [BootInstance]
                       } deriving(Eq,Show)

instance ToJSON BoxInfo where
    toJSON boxInfo = object [ "name"     .= boxName  boxInfo
                            , "mac"      .= mac      boxInfo
                            , "boot"     .= boot     boxInfo
                            , "bootlogs" .= bootlogs boxInfo
                            ]
instance FromJSON BoxInfo where
    parseJSON (Object v) = BoxInfo
                             <$> v .: "name"
                             <*> v .: "mac"
                             <*> v .: "boot"
                             <*> v .: "bootlogs"
    parseJSON _          = empty

data BootInstance = BootInstance { bootImg  :: T.Text
                                 , bootTime :: LocalTime
                                 } deriving(Eq,Show)

instance ToJSON BootInstance where
    toJSON (BootInstance img time) = object [ "image_name" .= img
                                            , "timestamp"  .= time
                                            ]
instance FromJSON BootInstance where
    parseJSON (Object v) = BootInstance
                             <$> v .: "image_name"
                             <*> v .: "timestamp"
    parseJSON _          = empty

data BootSettings = BootSettings { image :: T.Text
                                 , until :: LocalTime
                                 , flags :: [BootFlag]
                                 } deriving(Eq,Show)

instance ToJSON BootSettings where
    toJSON (BootSettings i u fs) = object [ "image" .= i
                                          , "until" .= u
                                          , "flags" .= fs
                                          ]
instance FromJSON BootSettings where
    parseJSON (Object v) = BootSettings
                             <$> v .: "image"
                             <*> v .: "until"
                             <*> v .: "flags"
    parseJSON _          = empty

data BootFlag = BootFlag { flagName  :: T.Text
                         , flagValue :: Maybe T.Text
                         } deriving(Eq,Show)

instance ToJSON BootFlag where
    toJSON (BootFlag name val) = object [ "name"  .= name
                                        , "value" .= val
                                        ]
instance FromJSON BootFlag where
    parseJSON (Object v) = BootFlag
                             <$> v .: "name"
                             <*> v .: "value"
    parseJSON _          = empty

data UploadID = UploadID { uploadID :: Int } deriving(Eq,Show)

instance ToJSON UploadID where
    toJSON (UploadID i) = object [ "id" .= i ]

instance FromJSON UploadID where
    parseJSON (Object v) = UploadID <$> v .: "id"
    parseJSON _          = empty

data NewImage = NewImage { newImgName :: T.Text } deriving(Eq,Show)

instance ToJSON NewImage where
    toJSON (NewImage name) = object [ "name" .= name ]

instance FromJSON NewImage where
    parseJSON (Object v) = NewImage <$> v .: "name"
    parseJSON _          = empty

data NewBox = NewBox { newBoxMac  :: T.Text
                     } deriving(Eq,Show)

instance ToJSON NewBox where
    toJSON (NewBox name) = object [ "name" .= name ]

instance FromJSON NewBox where
    parseJSON (Object v) = NewBox
                               <$> v .: "mac"
    parseJSON _          = empty

data UpdateBox = UpdateBox { bootImage :: Maybe T.Text
                           , bootUntil :: Maybe LocalTime
                           , bootFlags :: Maybe [BootFlag]
                           } deriving(Eq,Show)

instance ToJSON UpdateBox where
    toJSON (UpdateBox img til fs) = object [ "image"      .= img
                                           , "until"      .= til
                                           , "boot_flags" .= fs
                                           ]

instance FromJSON UpdateBox where
    parseJSON (Object v) = UpdateBox
                               <$> v .: "image"
                               <*> v .: "until"
                               <*> v .: "boot_flags"
    parseJSON _          = empty
