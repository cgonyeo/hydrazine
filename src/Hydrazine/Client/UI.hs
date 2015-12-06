{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Client.UI where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

import Servant.Client

import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Text as T
import qualified Data.Vector as V

import Text.Printf (printf)

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Lens ((^.),to)

import Hydrazine.JSON
import Hydrazine.Client.API

data BoxUntil = UntilNever
              | Until5Mins
              | Until30Mins
              | Until6Hours
              | UntilForever

data BoxDetailsState = ViewingBoxDetails
                     | PickingBoxImage (L.List (Maybe ImageInfo))
                     | SettingBoxUntil (D.Dialog BoxUntil)
                     | DeleteBox (D.Dialog Bool)

data HydrazineUI = BoxList         (L.List BoxInfo)
                 | BoxDetails      BoxInfo BoxDetailsState
                 | ImageList       (L.List ImageInfo)
                 | ImageDetails    ImageInfo
                 | NewImage
                 | NewImageWorking UploadID T.Text [T.Text]
                 | HydrazineError  ServantError
                 | Help

maxNameSize :: Int
maxNameSize = 16

formattedDateSize :: Int
formattedDateSize = 19

drawColumns :: [Widget] -> Widget
drawColumns []     = txt ""
drawColumns [x]    = x
drawColumns (x:xs) = padRight (Pad 2) x <+> drawColumns xs

hSpace :: Int -> Widget
hSpace x = txt $ T.replicate x " "

vSpace :: Int -> Widget
vSpace x = txt $ T.replicate x "\n"

showDate :: LocalTime -> String
showDate (LocalTime day (TimeOfDay h m s)) = show day ++ " "
                            ++ p h ++ ":" ++ p m ++ ":" ++ p (floor s :: Int)
    where p = printf "%02d"

data Justify = JustifyLeft
             | Center
             | JustifyRight

formatText :: Justify -> Int -> T.Text -> Widget
formatText JustifyLeft  size t = hLimit size $ txt $ T.justifyLeft  size ' ' t
formatText Center       size t = hLimit size $ txt $ T.center       size ' ' t
formatText JustifyRight size t = hLimit size $ txt $ T.justifyRight size ' ' t

-- Behaves the same as Brick.Widgets.Center.center, but without filling in
-- the background with spaces. Useful when layering widgets.
nonFillingCenter :: Widget -> Widget
nonFillingCenter w =
    Widget (hSize w) (vSize w) $ do
        result <- render w
        c <- getContext
        let availableWidth  = c^.availWidthL
            availableHeight = c^.availHeightL
            widgWidth       = result^.imageL.to V.imageWidth
            widgHeight      = result^.imageL.to V.imageHeight
            translateW      = (availableWidth `div` 2) - (widgWidth `div` 2)
            translateH      = (availableHeight `div` 2) - (widgHeight `div` 2)
        render $ translateBy (Location (translateW,translateH)) w
        
addTime :: TimeZone -> Int -> LocalTime -> LocalTime
addTime z s t = utcToLocalTime z $ addUTCTime (fromIntegral s) (localTimeToUTC z t)

addTime' :: Int -> LocalTime -> IO LocalTime
addTime' s t = do
        z <- getCurrentTimeZone
        return $ addTime z s t

getNow :: IO LocalTime
getNow = do
    z <- getCurrentTimeZone
    t <- getCurrentTime
    return $ utcToLocalTime z t

goToBoxList :: EventM (Next HydrazineUI)
goToBoxList = do
    eBoxen <- liftIO $ runEitherT getMachines
    case eBoxen of
        Left err    -> M.continue $ HydrazineError err
        Right boxen -> M.continue $ BoxList (L.list "boxen" (V.fromList boxen) 1)

goToImageList :: EventM (Next HydrazineUI)
goToImageList = do
    eImages <- liftIO $ runEitherT getImages
    case eImages of
        Left err     -> M.continue $ HydrazineError err
        Right images -> M.continue $ ImageList (L.list "images" (V.fromList images) 1)
