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
    let nameHdr = formatText JustifyLeft maxNameSize       "Name"
        macHdr  = formatText JustifyLeft macSize           "MAC Address"
        imgHdr  = formatText JustifyLeft maxNameSize       "Boot Image"
        tilHdr  = formatText JustifyLeft formattedDateSize "Boot Until"
    in drawColumns [nameHdr, macHdr, imgHdr, tilHdr]

macSize :: Int
macSize = 17

tilSize :: Int
tilSize = 16

drawBoxListUI :: L.List BoxInfo -> [Widget]
drawBoxListUI l = [ui]
    where
        label = txt "Machines"
        machines = L.renderList l listDrawBox
        instructions = drawColumns
            [ txt "enter: view machine"
            , txt "n: new machine"
            , txt "r: refresh"
            , txt "i: view images"
            ]

        ui = C.center
                $ hLimit 78
                $ vLimit 25
                $ vBox [ B.borderWithLabel label $ padAll 1 $ vBox
                            [ headers
                            , B.hBorder
                            , machines
                            ]
                       , C.hCenter $ instructions
                       ]

listDrawBox :: Bool -> BoxInfo -> Widget
listDrawBox _ (BoxInfo name macaddr mBootSettings _) =
    let widgName = formatText JustifyLeft maxNameSize name
        widgMac  = hLimit macSize (txt macaddr)
        widgImg  = formatText JustifyLeft maxNameSize $
            case mBootSettings of
                Just (BootSettings img _ _) -> img
                Nothing -> ""
        widgUntil = formatText JustifyLeft formattedDateSize $
            case mBootSettings of
                Just (BootSettings _ (BootUntil True _) _) ->
                     "Forever"
                Just (BootSettings _ (BootUntil _ Nothing) _) ->
                     "Never"
                Just (BootSettings _ (BootUntil _ (Just til)) _) ->
                     T.pack $ showDate til
                _ -> ""
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
                Just (_,box) -> M.continue $ BoxDetails box ViewingBoxDetails
                Nothing      -> M.continue $ BoxList l

        V.EvKey (V.KChar 'r') [] -> goToBoxList

        V.EvKey (V.KChar 'i') [] -> goToImageList

        ev -> do
            l' <- handleEvent ev l
            M.continue $ BoxList l'
