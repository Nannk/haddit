module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Themes
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import System.Posix.Internals (statGetType)
import Graphics.Vty (standout)

tui :: IO()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState =
    TuiState
    deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp = 
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure 
        , appAttrMap = const $ attrMap mempty []
        }

buildInitialState :: IO TuiState
buildInitialState = pure TuiState

drawTui :: TuiState -> [Widget ResourceName]
drawTui _ts = []

handleTuiEvent :: TuiState -> BrickEvent name event -> EventM name (Next TuiState)
handleTuiEvent state event =
    case event of
      VtyEvent vtyevent -> 
          case vtyevent of 
            EvKey (KChar 'q') [] -> halt state
            _-> continue state
      _-> continue state




