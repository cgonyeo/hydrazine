{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.JSON where

import Data.Aeson
import Data.Time.Calendar
import Control.Applicative
import Data.Time.LocalTime
import Data.Char
import Servant
import Control.Monad.Trans.Either

import qualified Data.Text as T

data BootInfo = BootInfo
        { kernel  :: T.Text
        , initrd  :: [T.Text]
        , cmdline :: Value
        } deriving(Eq,Show)

data ImageInfo = ImageInfo
        { imgName      :: T.Text            -- JSON: name
        , created      :: LocalTime
        , imgKernel    :: T.Text            -- JSON: kernel
        , cpios        :: [T.Text]
        , defaultflags :: [BootFlag]        -- JSON: default_flags
        } deriving(Eq,Show)

data BoxInfo = BoxInfo
        { boxName  :: T.Text                -- JSON: name
        , mac      :: T.Text
        , boot     :: Maybe BootSettings
        , bootlogs :: [BootInstance]
        } deriving(Eq,Show)

data BootInstance = BootInstance
        { bootImg  :: T.Text                -- JSON: image_name
        , bootTime :: LocalTime             -- JSON: timestamp
        } deriving(Eq,Show)

data BootSettings = BootSettings
        { image :: T.Text
        , until :: BootUntil
        , flags :: [BootFlag]
        } deriving(Eq,Show)

data BootFlag = BootFlag
        { flagName  :: T.Text               -- JSON: name
        , flagValue :: Maybe T.Text         -- JSON: value
        } deriving(Eq,Show)

data UploadID = UploadID
        { uploadID :: Int                   -- JSON: id
        } deriving(Eq,Show)

data NewImage = NewImage
        { newImgName :: T.Text              -- JSON: name
        } deriving(Eq,Show)

data NewBox = NewBox
        { newBoxMac  :: T.Text              -- JSON: mac
        } deriving(Eq,Show)

data BootUntil = BootUntil
        { untilForever  :: Bool             -- JSON: forever
        , untilThisTime :: Maybe LocalTime  -- JSON: time
        } deriving(Eq,Show)

data UpdateBox = UpdateBox
        { bootImage :: Maybe T.Text         -- JSON: image
        , bootUntil :: Maybe BootUntil      -- JSON: until
        , bootFlags :: Maybe [BootFlag]     -- JSON: boot_flags
        } deriving(Eq,Show)

aTime :: LocalTime
aTime = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)

stripMac :: T.Text -> T.Text
stripMac = T.filter (isAlphaNum) . T.toUpper

formatMac :: T.Text -> T.Text
formatMac m 
  | T.length m <= 2 = m
  | otherwise       = (T.toUpper $ T.take 2 m) `T.append` ":" `T.append` formatMac (T.drop 2 m)

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

instance ToJSON BootInstance where
    toJSON (BootInstance img time) = object [ "image_name" .= img
                                            , "timestamp"  .= time
                                            ]
instance FromJSON BootInstance where
    parseJSON (Object v) = BootInstance
                             <$> v .: "image_name"
                             <*> v .: "timestamp"
    parseJSON _          = empty

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

instance ToJSON BootFlag where
    toJSON (BootFlag name val) = object [ "name"  .= name
                                        , "value" .= val
                                        ]
instance FromJSON BootFlag where
    parseJSON (Object v) = BootFlag
                             <$> v .: "name"
                             <*> v .: "value"
    parseJSON _          = empty

instance ToJSON UploadID where
    toJSON (UploadID i) = object [ "id" .= i ]

instance FromJSON UploadID where
    parseJSON (Object v) = UploadID <$> v .: "id"
    parseJSON _          = empty

instance ToJSON NewImage where
    toJSON (NewImage name) = object [ "name" .= name ]

instance FromJSON NewImage where
    parseJSON (Object v) = NewImage <$> v .: "name"
    parseJSON _          = empty

instance ToJSON NewBox where
    toJSON (NewBox name) = object [ "name" .= name ]

instance FromJSON NewBox where
    parseJSON (Object v) = NewBox
                               <$> v .: "mac"
    parseJSON _          = empty

instance ToJSON BootUntil where
    toJSON (BootUntil f t) = object [ "forever" .= f
                                    , "time"    .= t
                                    ]

instance FromJSON BootUntil where
    parseJSON (Object v) = BootUntil
                               <$> v .: "forever"
                               <*> v .: "time"
    parseJSON _          = empty

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

data EmptyValue = EmptyValue Int

instance ToJSON EmptyValue where
    toJSON (EmptyValue sdf) = object ["sdf" .= sdf]
instance FromJSON EmptyValue where
    parseJSON (Object v) = EmptyValue <$> v .: "sdf"
    parseJSON _          = empty

returnEmptyValue :: a -> EitherT ServantErr IO EmptyValue
returnEmptyValue _ = right $ EmptyValue 1
