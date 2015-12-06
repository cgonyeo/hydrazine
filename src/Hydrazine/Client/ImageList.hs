{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Client.ImageList where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import qualified Data.Text as T

import Hydrazine.JSON
import Hydrazine.Client.API
import Hydrazine.Client.UI

headers :: Widget
headers =
    let nameHdr    = formatText JustifyLeft maxNameSize       "Name"
        createdHdr = formatText JustifyLeft formattedDateSize "Created"
    in drawColumns [nameHdr, createdHdr]

macSize :: Int
macSize = 17

tilSize :: Int
tilSize = 16

drawImageListUI :: L.List ImageInfo -> [Widget]
drawImageListUI l = [ui]
    where
        label = txt "Images"
        images = L.renderList l listDrawImage
        instructions = drawColumns
                [ txt "enter: view image"
                , txt "n: new image"
                ]
            <=> drawColumns
                [ txt "r: refresh"
                , txt "m: view machines"
                ]

        ui = C.center
                $ hLimit 41
                $ vLimit 25
                $ vBox [ B.borderWithLabel label $ padAll 1 $ vBox
                            [ headers
                            , B.hBorder
                            , images
                            ]
                       , C.hCenter $ instructions
                       ]

listDrawImage :: Bool -> ImageInfo -> Widget
listDrawImage _ (ImageInfo name c _ _ _) =
    let widgName    = formatText JustifyLeft maxNameSize name
        widgCreated = formatText JustifyLeft formattedDateSize (T.pack $ showDate c)
    in drawColumns [widgName, widgCreated]

handleImageListEvent :: L.List ImageInfo -> V.Event -> EventM (Next HydrazineUI)
handleImageListEvent l e =
    case e of
        V.EvKey (V.KChar 'j') [] -> M.continue $ ImageList (L.listMoveDown l)
        V.EvKey (V.KChar 'k') [] -> M.continue $ ImageList (L.listMoveUp l)

        V.EvKey V.KEsc []               -> M.halt (ImageList l)
        V.EvKey (V.KChar 'q') []        -> M.halt (ImageList l)
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt (ImageList l)

        V.EvKey V.KEnter [] -> do
            case L.listSelectedElement l of
                Just (_,img) -> M.continue $ ImageDetails img
                Nothing      -> M.continue $ ImageList l

        V.EvKey (V.KChar 'r') [] -> goToImageList

        V.EvKey (V.KChar 'm') [] -> goToBoxList

        ev -> do
            l' <- handleEvent ev l
            M.continue $ ImageList l'
