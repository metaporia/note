{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro
import Lens.Micro.TH
import Graphics.Vty

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.AttrMap
import Brick.Focus
import Brick.Widgets.Border
import Brick.Util (on)

import Control.Monad.IO.Class

import qualified Text.Trifecta as Tri
import qualified Data.Text as T

import UI.Service
import UI.Types hiding (Cmd)
import qualified UI.Types as T
import Note (note')

import UI.REPL (parse, pCmd)

type Cmd' = T.Cmd

data Name = Cmd
          | SubjCtx
          deriving (Ord, Show, Eq)

data St =
    St { _foci :: FocusRing Name
       , _cmd :: Editor String Name
       --, _edit2 :: Editor String Name
       , _subjCtx :: String
       --, _note :: NoteS String 
       }

makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st = [ui]
    where
        e1 = withFocusRing (st^.foci) (renderEditor (str . unlines)) (st^.cmd)
        --e2 = withFocusRing (st^.focusRing) (renderEditor (str . unlines)) (st^.edit2)
        d0 = str (st^.subjCtx)

        ui = center $
            (padRight (Pad 10) ( padLeft (Pad 0) (border d0))) <=>
            (hLimit 70 $ vLimit 10 e1)

appEvent :: St -> BrickEvent Name e -> EventM Name (Next St)
appEvent st (VtyEvent ev) =
    let s = unlines . getEditContents $ (st^.cmd) in
    case ev of
        EvKey KEsc [] -> halt st
--        EvKey (KChar '\t') [] -> continue $ st & foci %~ focusNext
 --       EvKey KEnter [] -> continue $ st & foci %~ focusPrev
        EvKey (KChar 'l') [MCtrl] -> continue $ set subjCtx (replicate 24 ' ') st
        EvKey  KEnter [] -> continue $ set subjCtx s st
        -- ^ parse cmd for instruction pCmd
        
        EvKey (KChar 'f') [MCtrl] -> do f <- liftIO $ readFile (T.unpack $ T.strip $ T.pack s)
                                        liftIO $ print (T.unpack $ T.strip $ T.pack s)
                                        continue $ set subjCtx f st
                                        --continue st
        EvKey (KChar 'p') [MCtrl] -> do let str = case parse pCmd s of
                                                  Tri.Success c -> show c
                                                  Tri.Failure err -> show err
                                        continue $ set subjCtx str st
                                                



        _ -> continue =<< case focusGetCurrent (st^.foci) of
               Just Cmd -> handleEventLensed st cmd handleEditorEvent ev
               --Just Edit2 -> handleEventLensed st edit2 handleEditorEvent ev
               Nothing -> return st
appEvent st _ = continue st

initialState :: St
initialState =
    St (focusRing [Cmd])
       (editor Cmd (Just 1) "")
       --(editor Edit2 (Just 2) "")
       ""

theMap :: AttrMap
theMap = attrMap defAttr
    [ (editAttr,                   red `on` black)
    , (editFocusedAttr,            black `on` white)
    ]

appCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = focusRingCursor (^.foci)

theApp :: App St e Name
theApp =
    App { appDraw = drawUI
          , appChooseCursor = appCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
    st <- defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ getEditContents $ st^.cmd
    putStrLn "In input 2 you entered:\n"
--    putStrLn $ unlines $ getEditContents $ st^.edit2
--
main' :: FilePath -> IO ()
main' s = do
    f <- readFile s
    -- load f
    main
