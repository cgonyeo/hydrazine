{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

import Servant.Client

import qualified Data.Vector as V
import qualified Data.Text as T

import Control.Monad (void)
import Control.Monad.Trans.Either

import Hydrazine.JSON
import Hydrazine.Client.API
import Hydrazine.Client.UI
import Hydrazine.Client.BoxList
import Hydrazine.Client.BoxDetails
import Hydrazine.Client.ImageList

main :: IO ()
main = do
    eBoxen <- runEitherT getMachines
    case eBoxen of
        Left err    -> void $ M.defaultMain theApp (HydrazineError err)
        Right boxen -> void $ M.defaultMain theApp (newBoxList boxen)

theApp :: M.App HydrazineUI V.Event
theApp =
    M.App { M.appDraw         = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = appEvent
          , M.appStartEvent   = return
          , M.appAttrMap      = const theMap
          , M.appLiftVtyEvent = id
          }

drawUI :: HydrazineUI -> [Widget]
drawUI (BoxList l)        = drawBoxListUI l
drawUI (BoxDetails b s)   = drawBoxDetailsUI b s
drawUI (ImageList l)      = drawImageListUI l
drawUI (HydrazineError e) = drawError e

appEvent :: HydrazineUI -> V.Event -> EventM (Next HydrazineUI)
appEvent (BoxList l) e          = handleBoxListEvent l e
appEvent (BoxDetails b s) e     = handleBoxDetailEvent b s e
appEvent (ImageList l) e        = handleImageListEvent l e
appEvent (HydrazineError err) e = M.halt $ HydrazineError err

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,         V.white `on` V.black)
    , (L.listSelectedAttr, V.black `on` V.white)
    , (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.white `on` V.black)
    , (D.buttonSelectedAttr, V.black `on` V.white)
    ]

drawError :: ServantError -> [Widget]
drawError err = [C.center $ vBox $ map txt $ T.chunksOf 80 $ T.pack $ show err]

boxListName :: Name
boxListName = Name "machines"

newBoxList :: [BoxInfo] -> HydrazineUI
newBoxList boxen = BoxList $ L.list boxListName (V.fromList boxen) 1
