{-# LANGUAGE OverloadedStrings #-}

module Hydrazine.Client.BoxDetails where

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

import Servant.Client

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V

import Hydrazine.JSON
import Hydrazine.Client.API
import Hydrazine.Client.UI

drawBoxDetailsUI :: BoxInfo -> BoxDetailsState -> [Widget]
drawBoxDetailsUI b ViewingBoxDetails   = [ drawBoxDetails b ViewingBoxDetails ]
drawBoxDetailsUI b (SettingBoxUntil d) = [ drawTilDialog (boxName b) d ]
drawBoxDetailsUI b (DeleteBox d)       = [ drawDelDialog (boxName b) d ]
drawBoxDetailsUI b (PickingBoxImage l) = [ drawImagePicker l
                                         , drawBoxDetails b (PickingBoxImage l)
                                         ]

drawTilDialog :: T.Text -> D.Dialog BoxUntil -> Widget
drawTilDialog name d =  D.renderDialog d $ C.hCenter $ padAll 1
    $ txt ("Boot " `T.append` name `T.append` " with this image for how long?")

newTilDialog :: D.Dialog BoxUntil
newTilDialog = D.dialog
                "tildialog"
                Nothing
                (Just (0,dialogMappings))
                80

dialogMappings :: [(String,BoxUntil)]
dialogMappings =
        [ ("Never",UntilNever)
        , ("5 Minutes",Until5Mins)
        , ("30 Minutes",Until30Mins)
        , ("6 Hours",Until6Hours)
        , ("Forever",UntilForever)
        ]

drawDelDialog :: T.Text -> D.Dialog Bool -> Widget
drawDelDialog name d = D.renderDialog d $ C.hCenter $ padAll 1
    $ txt ("Are you sure you want to delete " `T.append` name `T.append` "?")

newDelDialog :: D.Dialog Bool
newDelDialog = D.dialog
                "deldialog"
                (Just "Please Confirm")
                (Just (0,[("No",False),("Yes",True)]))
                60

drawImagePicker :: L.List (Maybe ImageInfo) -> Widget
drawImagePicker l = ui
    where
        label = txt "Images"
        headers = drawColumns [ formatText JustifyLeft maxNameSize "Name"
                              , formatText JustifyLeft formattedDateSize "Created"
                              ]
        images = L.renderList l listDrawImage
        ui = nonFillingCenter
                $ hLimit 41
                $ vLimit 20
                $ B.borderWithLabel label
                $ padAll 1
                $ vBox [ headers
                       , vSpace 1
                       , images
                       ]

listDrawImage :: Bool -> (Maybe ImageInfo) -> Widget
listDrawImage _ (Just (ImageInfo name c _ _ _)) =
    drawColumns [ formatText JustifyLeft maxNameSize name
                , str $ showDate c
                ]
listDrawImage _ Nothing = txt "Nothing"

drawBoxDetails :: BoxInfo -> BoxDetailsState -> Widget
drawBoxDetails b s =
    C.center
        $ hLimit 80
        $ vLimit 25
        $ vBox [ B.borderWithLabel (txt "Machines") $ hBox
                    [ C.hCenter $ vBox
                           [ C.hCenter $ padAll 1 $ txt "Machine Information"
                           , C.hCenter $ padAll 1 $ drawLeftPane b
                           ]
                    , B.vBorder
                    , hLimit 39 $ vBox
                           [ C.hCenter $ padAll 1 $ txt "Boot Logs"
                           , C.hCenter $ padAll 1 $ drawRightPane b
                           ]
                    ]
               , C.hCenter $ instructions s
               ]

instructions :: BoxDetailsState -> Widget
instructions ViewingBoxDetails =
    drawColumns [ txt "i: set image"
                , txt "t: set boot until"
                , txt "f: edit boot flags"
                , txt "d: delete machine"
                ]
instructions (PickingBoxImage _) =
    drawColumns [ txt "j/k: move cursor"
                , txt "enter: pick machine"
                ]
instructions _ = txt ""

drawLeftPane :: BoxInfo -> Widget
drawLeftPane (BoxInfo name macaddr mboot _) = leftPane
    where
        infoLabels = [ txt "       Name"
                     , txt "MAC Address"
                     ]

        infoValues = [ txt name
                     , txt macaddr
                     ]

        (infoLabels',infoValues') =
            case mboot of
                Nothing -> (infoLabels,infoValues)
                Just (BootSettings img bootTil bflags) ->
                    ( infoLabels
                        ++ [ txt "Boot Image"
                           , txt "Boot Until"
                           , txt "Boot Flags"
                           ]
                    , infoValues
                        ++ [ txt img
                           , case bootTil of
                                 BootUntil True _ -> txt "Forever"
                                 BootUntil _ Nothing -> txt "Never"
                                 BootUntil _ (Just til) -> str $ showDate til
                           , drawBootFlags bflags
                           ]
                    )

        leftPane = hBox [ vBox $ map (padBottom (Pad 1)) infoLabels'
                        , hSpace 2
                        , vBox $ map (padBottom (Pad 1)) infoValues'
                        ]

drawBootFlags :: [BootFlag] -> Widget
drawBootFlags bflags = vBox $ map toWidg bflags
    where toWidg (BootFlag n (Just v)) = txt $ n `T.append` "=" `T.append` v
          toWidg (BootFlag n Nothing)  = txt n

drawRightPane :: BoxInfo -> Widget
drawRightPane (BoxInfo _ _ _ logs) = rightPane
    where 
        col1Size = 16
        col2Size = 19
        headers = drawColumns [ formatText JustifyLeft col1Size "Image Name"
                              , formatText JustifyLeft col2Size "Timestamp"
                              ]
        rows = map (\(BootInstance img t)
                     -> hBox [ formatText JustifyLeft col1Size img
                             , hSpace 2
                             , formatText JustifyRight col2Size (T.pack $ showDate t)
                             ]
                   ) logs
        rightPane = vBox [ headers
                         , vSpace 1
                         , vBox rows
                         ]
                
handleBoxDetailEvent :: BoxInfo -> BoxDetailsState -> V.Event -> EventM (Next HydrazineUI)
handleBoxDetailEvent b (ViewingBoxDetails) e =
    case e of
        V.EvKey V.KEsc []        -> goToBoxList
        V.EvKey (V.KChar 'q') [] -> goToBoxList

        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt (BoxDetails b ViewingBoxDetails)

        V.EvKey (V.KChar 'i') [] -> do
            eImages <- liftIO $ runEitherT getImages
            case eImages of
                Left err -> retError err
                Right images -> M.continue $ BoxDetails b
                    (PickingBoxImage (L.list "Images" (V.fromList (Nothing : map Just images)) 1))

        V.EvKey (V.KChar 't') [] -> M.continue $ BoxDetails b (SettingBoxUntil newTilDialog)

        V.EvKey (V.KChar 'd') [] -> M.continue $ BoxDetails b (DeleteBox newDelDialog)

        _ -> M.continue $ BoxDetails b ViewingBoxDetails
handleBoxDetailEvent b (PickingBoxImage l) e =
    case e of
        V.EvKey V.KEsc []        -> M.continue $ BoxDetails b ViewingBoxDetails
        V.EvKey (V.KChar 'q') [] -> M.continue $ BoxDetails b ViewingBoxDetails

        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt $ BoxDetails b (PickingBoxImage l)

        V.EvKey (V.KChar 'j') [] -> M.continue $ BoxDetails b
                                        (PickingBoxImage $ L.listMoveDown l)
        V.EvKey (V.KChar 'k') [] -> M.continue $ BoxDetails b
                                        (PickingBoxImage $ L.listMoveUp l)

        V.EvKey V.KEnter [] ->
            case L.listSelectedElement l of
                Nothing        -> M.continue $ BoxDetails b (PickingBoxImage l)
                Just (_,i) -> do
                    let i' = if isJust i then imgName $ fromJust i else ""
                    res <- liftIO $ runEitherT $ updateMachine (boxName b)
                                        (UpdateBox (Just i') Nothing Nothing)
                    eBox <- liftIO $ runEitherT $ getMachine (boxName b)
                    case (res,eBox) of
                        (Left err,_) -> retError err
                        (_,Left err) -> retError err
                        (_,Right box) -> M.continue $ BoxDetails box ViewingBoxDetails

        ev -> do
            l' <- handleEvent ev l
            M.continue $ BoxDetails b (PickingBoxImage l')
handleBoxDetailEvent b (SettingBoxUntil d) e =
    case e of
        V.EvKey V.KEsc []        -> M.continue $ BoxDetails b ViewingBoxDetails
        V.EvKey (V.KChar 'q') [] -> M.continue $ BoxDetails b ViewingBoxDetails

        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt $ BoxDetails b (SettingBoxUntil d)

        V.EvKey V.KEnter [] ->
            case D.dialogSelectedIndex d >>= (\i -> Just $ snd $ dialogMappings !! i) of
                Nothing -> M.continue $ BoxDetails b (SettingBoxUntil d)
                Just val -> do
                    now <- liftIO getNow
                    let nowPlus t   = liftIO $ addTime' t now
                    newTil <- case val of
                        UntilNever  -> return (BootUntil False Nothing)
                        Until5Mins  -> (BootUntil False . Just ) <$> nowPlus (5 * 60)
                        Until30Mins -> (BootUntil False . Just ) <$> nowPlus (30 * 60)
                        Until6Hours -> (BootUntil False . Just ) <$> nowPlus (6 * 60 * 60)
                        UntilForever -> return (BootUntil True (Just aTime))
                    res <- liftIO $ runEitherT $ updateMachine (boxName b)
                                    (UpdateBox Nothing (Just newTil) Nothing)
                    eBox <- liftIO $ runEitherT $ getMachine (boxName b)
                    case (res, eBox) of
                        (Left err,_) -> retError err
                        (_,Left err) -> retError err
                        (_,Right box) -> M.continue $ BoxDetails box ViewingBoxDetails

        ev -> do
            d' <- handleEvent ev d
            M.continue $ BoxDetails b (SettingBoxUntil d')
handleBoxDetailEvent b (DeleteBox d) e =
    case e of
        V.EvKey V.KEsc []        -> M.continue $ BoxDetails b ViewingBoxDetails
        V.EvKey (V.KChar 'q') [] -> M.continue $ BoxDetails b ViewingBoxDetails

        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt $ BoxDetails b (DeleteBox d)

        V.EvKey V.KEnter [] ->
            case D.dialogSelectedIndex d  of
                Nothing -> M.continue $ BoxDetails b (DeleteBox d)
                Just val -> do
                    case val of
                        0 -> do
                            eBox <- liftIO $ runEitherT $ getMachine (boxName b)
                            case eBox of
                                Left err  -> retError err
                                Right box -> M.continue $ BoxDetails box ViewingBoxDetails
                        1 -> do
                            eErr <- liftIO $ runEitherT $ deleteMachine (boxName b)
                            case eErr of
                                Left err -> retError err
                                Right _ -> goToBoxList
                        _ -> M.continue $ BoxDetails b (DeleteBox d)

        ev -> do
            d' <- handleEvent ev d
            M.continue $ BoxDetails b (DeleteBox d')

retError :: ServantError -> EventM (Next HydrazineUI)
retError = M.continue . HydrazineError
