module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Themes
import Brick.Widgets.Core
import Brick.Forms
import Graphics.Vty.Input.Events as IE
import System.Posix.Internals (statGetType)
import Graphics.Vty (standout)
import Network.Curl

-- str 
-- vBox 

startPageUI :: String
startPageUI ="Search for a subreddit"

tui :: IO()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState = TuiState
    {tuiStateGreeting :: [String]}
    deriving(Show, Eq)

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
buildInitialState = do
    pure TuiState {tuiStateGreeting = startPageUI:[]}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [vBox $ map str $ tuiStateGreeting ts]

handleTuiEvent :: TuiState -> BrickEvent name event -> EventM name (Next TuiState)
handleTuiEvent state event =
    case event of
      VtyEvent vtyevent -> 
          case vtyevent of 
            EvKey (KChar 'q') [] -> halt state
            _-> continue state
      _-> continue state




