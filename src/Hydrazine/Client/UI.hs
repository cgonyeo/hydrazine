module Hydrazine.Client.UI where

import Brick
import qualified Brick.Widgets.List as L

import Servant.Client

import qualified Data.Text as T
import qualified Data.Vector as V

import Hydrazine.JSON

data HydrazineUI = BoxList           (L.List BoxInfo)
                 | BoxListError      ServantError
                 | BoxDetails        BoxInfo
                 | BoxDetailsError   ServantError
                 | ImageList         (L.List ImageInfo)
                 | ImageListError    ServantError
                 | ImageDetails      ImageInfo
                 | ImageDetailsError ServantError
                 | NewImage
                 | NewImageWorking   UploadID T.Text [T.Text]
                 | NewImageError     ServantError
                 | Help

boxListName :: Name
boxListName = Name "machines"

newBoxList :: [BoxInfo] -> HydrazineUI
newBoxList boxen = BoxList $ L.list boxListName (V.fromList boxen) 1
