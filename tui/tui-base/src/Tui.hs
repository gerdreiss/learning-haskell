{-# LANGUAGE OverloadedStrings #-}

module Tui where

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.Widgets.Core
import           Control.Monad.IO.Class
import           Cursor.Simple.List.NonEmpty
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           System.Directory
import           System.Exit
import           System.IO


tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState     <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState
  { tuiStatePaths :: NonEmptyCursor FilePath
  , tuiStateFileContent :: String
  }
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const $ attrMap mempty [("selected", fg red)]
             }

buildInitialState :: IO TuiState
buildInitialState = do
  here     <- getCurrentDirectory
  contents <- getDirectoryContents here
  case NE.nonEmpty contents of
    Nothing -> die "There are no contents."
    Just ne -> pure TuiState
      { tuiStatePaths       = makeNonEmptyCursor ne
      , tuiStateFileContent = "select  file to display its contents here"
      }

drawTui :: TuiState -> [Widget ResourceName]
drawTui _ts =
  let nec = tuiStatePaths _ts
      txt = tuiStateFileContent _ts
  in  [ B.border $ hBox
          [ withBorderStyle BS.unicodeRounded
          $ B.borderWithLabel (str " file system ")
          $ hLimit 20
          $ C.vCenter
          $ C.hCenter
          $ vBox
          $ concat
              [ map (drawPath False) . reverse . nonEmptyCursorPrev $ nec
              , [drawPath True $ nonEmptyCursorCurrent nec]
              , map (drawPath False) $ nonEmptyCursorNext nec
              ]
          , withBorderStyle BS.unicodeRounded
          $ B.borderWithLabel (str " file content ")
          $ vBox [str txt]
          ]
      ]

drawPath :: Bool -> FilePath -> Widget n
drawPath b = (if b then withAttr "selected" else id) . str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey KDown       [] -> do
      let nec = tuiStatePaths s
      case nonEmptyCursorSelectNext nec of
        Nothing   -> continue s
        Just nec' -> continue $ s { tuiStatePaths = nec' }
    EvKey KUp [] -> do
      let nec = tuiStatePaths s
      case nonEmptyCursorSelectPrev nec of
        Nothing   -> continue s
        Just nec' -> continue $ s { tuiStatePaths = nec' }
    EvKey KEnter [] -> do
      let selected = nonEmptyCursorCurrent $ tuiStatePaths s
      isDirectory <- liftIO $ doesDirectoryExist selected
      if isDirectory
        then do
          liftIO . setCurrentDirectory $ selected
          s' <- liftIO buildInitialState
          continue s'
        else do
          contents <- liftIO $ readFile selected
          continue s { tuiStateFileContent = contents }
    _ -> continue s
  _ -> continue s

