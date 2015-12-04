{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Client.BoxList where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Vector as V

import Hydrazine.JSON
import Hydrazine.Client.API
import Hydrazine.Client.UI

headers :: Widget
headers =
    let nameHdr = txt $ T.justifyLeft nameSize ' ' "Name"
        macHdr  = txt $ T.justifyLeft macSize  ' ' "MAC Address"
        imgHdr  = txt $ T.justifyLeft imgSize  ' ' "Boot Image"
        tilHdr  = txt $ T.justifyLeft tilSize  ' ' "Boot Until"
    in drawColumns [nameHdr, macHdr, imgHdr, tilHdr]

drawColumns :: [Widget] -> Widget
drawColumns []     = txt ""
drawColumns [x]    = x
drawColumns (x:xs) = padRight (Pad 2) x <+> drawColumns xs

nameSize :: Int
nameSize = 19

macSize :: Int
macSize = 17

imgSize ::Int
imgSize = 18

tilSize :: Int
tilSize = 16

drawBoxListUI :: L.List BoxInfo -> [Widget]
drawBoxListUI l = [ui]
    where
        label = txt "Machines"
        machines = L.renderList l listDrawBox
        instructions = drawColumns
            [ txt "j/k: move cursor"
            , txt "enter: view machine"
            , txt "r: refresh"
            , txt "i: view images"
            ]
        
        ui = C.center
                $ hLimit 80
                $ vLimit 25
                $ vBox [ B.borderWithLabel label $ padAll 1 (headers <=> machines)
                       , C.hCenter $ instructions
                       ]

listDrawBox :: Bool -> BoxInfo -> Widget
listDrawBox _ (BoxInfo name macaddr mBootSettings _) =
    let widgName = hLimit nameSize (txt $ T.justifyLeft nameSize ' ' name)
        widgMac  = hLimit macSize (txt macaddr)
        widgImg  = case mBootSettings of
                       Nothing -> txt $ T.replicate imgSize " "
                       Just (BootSettings img _ _) ->
                           hLimit imgSize (txt $ T.justifyLeft imgSize ' ' img)
        widgUntil = case mBootSettings of
                        Just (BootSettings _ (Just til) _) ->
                            hLimit tilSize (txt $ T.justifyLeft tilSize ' ' (T.pack $ show til))
                        _ -> txt ""
    in drawColumns [widgName, widgMac, widgImg, widgUntil]

handleBoxListEvent :: L.List BoxInfo -> V.Event -> EventM (Next HydrazineUI)
handleBoxListEvent l e =
    case e of
        V.EvKey (V.KChar 'j') [] -> M.continue $ BoxList (L.listMoveDown l)
        V.EvKey (V.KChar 'k') [] -> M.continue $ BoxList (L.listMoveUp l)

        V.EvKey V.KEsc []               -> M.halt (BoxList l)
        V.EvKey (V.KChar 'q') []        -> M.halt (BoxList l)
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt (BoxList l)

        V.EvKey V.KEnter [] -> do
            case L.listSelectedElement l of
                Just (_,box) -> M.continue $ BoxDetails box
                Nothing -> M.continue $ BoxList l

        V.EvKey (V.KChar 'r') [] -> do
            eBoxen <- liftIO $ runEitherT getMachines
            case eBoxen of
                Left err    -> M.continue $ BoxListError err
                Right boxen -> M.continue $ BoxList (L.listReplace (V.fromList boxen) l)

        ev -> do
            l' <- handleEvent ev l
            M.continue $ BoxList l'
