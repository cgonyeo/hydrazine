{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Client.BoxDetails where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Vector as V

import Hydrazine.JSON
import Hydrazine.Client.API
import Hydrazine.Client.UI

drawBoxDetailsUI :: BoxInfo -> [Widget]
drawBoxDetailsUI (BoxInfo name macaddr mBoot logs) = [ui]
    where
        label = txt name
        ui = label

handleBoxDetailEvent :: BoxInfo -> V.Event -> EventM (Next HydrazineUI)
handleBoxDetailEvent b e =
    case e of
        V.EvKey V.KEsc [] -> do
            eBoxen <- liftIO $ runEitherT getMachines
            case eBoxen of
                Left err    -> M.continue $ BoxListError err
                Right boxen -> M.continue $ BoxList (L.list boxListName (V.fromList boxen) 1)
        _ -> M.continue $ BoxDetails b
