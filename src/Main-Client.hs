{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Control.Monad.Trans.Either

import Brick

import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Data.Vector as V
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

import Hydrazine.Client.API
import Hydrazine.JSON

main :: IO ()
main = do
    eBoxen <- runEitherT getMachines
    case eBoxen of
        Left err -> putStrLn ("Error getting list of machines: " ++ show err)
        Right boxen -> do
            void $ M.defaultMain theApp (initialState boxen)

theApp :: M.App (L.List BoxInfo) V.Event
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

initialState :: [BoxInfo] -> L.List BoxInfo
initialState boxen = L.list (Name "machines") (V.fromList boxen) 1


drawUI :: L.List BoxInfo -> [Widget]
drawUI l = [ui]
    where
        label = txt "Machines"
        machines = L.renderList l listDrawBox
        
        ui = C.vCenter $ B.borderWithLabel label $ padAll 1 (headers <=> machines)

listDrawBox :: Bool -> BoxInfo -> Widget
listDrawBox sel (BoxInfo name mac mBootSettings _) =
    let widgName = hLimit nameSize (txt $ T.justifyLeft nameSize ' ' name)
        widgMac  = hLimit macSize (txt mac)
        widgImg  = case mBootSettings of
                       Nothing -> txt $ T.replicate imgSize " "
                       Just (BootSettings img _ _) ->
                           hLimit imgSize (txt $ T.justifyLeft imgSize ' ' img)
        widgUntil = case mBootSettings of
                        Just (BootSettings _ (Just til) _) ->
                            hLimit imgSize (txt $ T.justifyLeft tilSize ' ' (T.pack $ show til))
                        _ -> txt ""
    in drawColumns [widgName, widgMac, widgImg, widgUntil]

headers :: Widget
headers =
    let nameHdr = txt $ T.justifyLeft nameSize ' ' "Name"
        macHdr  = txt $ T.justifyLeft macSize  ' ' "MAC Address"
        imgHdr  = txt $ T.justifyLeft imgSize  ' ' "Boot Image"
        tilHdr  = txt $ T.justifyLeft tilSize  ' ' "Boot Until"
    in drawColumns [nameHdr, macHdr, imgHdr, tilHdr]

drawColumns :: [Widget] -> Widget
drawColumns (x:[]) = x
drawColumns (x:xs) = padRight (Pad 2) x <+> drawColumns xs

nameSize :: Int
nameSize = 16

macSize :: Int
macSize = 17

imgSize ::Int
imgSize = 16

tilSize :: Int
tilSize = 16

appEvent :: L.List BoxInfo -> V.Event -> EventM (Next (L.List BoxInfo))
appEvent l e =
    case e of
        V.EvKey (V.KChar 'j') [] -> M.continue $ L.listMoveDown l
        V.EvKey (V.KChar 'k') [] -> M.continue $ L.listMoveUp l

        V.EvKey V.KEsc []        -> M.halt l
        V.EvKey (V.KChar 'q') [] -> M.halt l

        ev -> M.continue =<< handleEvent ev l

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,         V.white `on` V.black)
    , (L.listSelectedAttr, V.black `on` V.white)
    ]
