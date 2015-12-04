{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import qualified Graphics.Vty as V
import Control.Monad.Trans.Either

import Brick

import qualified Brick.Main as M
import qualified Brick.Widgets.List as L

import Hydrazine.Client.API
import Hydrazine.Client.UI
import Hydrazine.Client.BoxList
import Hydrazine.Client.BoxDetails

main :: IO ()
main = do
    eBoxen <- runEitherT getMachines
    case eBoxen of
        Left err    -> void $ M.defaultMain theApp (BoxListError err)
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
drawUI (BoxList l)    = drawBoxListUI l
drawUI (BoxDetails b) = drawBoxDetailsUI b

appEvent :: HydrazineUI -> V.Event -> EventM (Next HydrazineUI)
appEvent (BoxList l) e    = handleBoxListEvent l e
appEvent (BoxDetails b) e = handleBoxDetailEvent b e

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,         V.white `on` V.black)
    , (L.listSelectedAttr, V.black `on` V.white)
    ]
