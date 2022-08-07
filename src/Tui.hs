module Tui where
import Cursor.Simple.List.NonEmpty

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

    -- Data Section 
-- test data
startPageUI :: String
startPageUI ="Search for a subreddit"

        -- every comment in this section is tui related, no web stuff
    -- UIInputs: user -> pc 
-- 1 Hotkeys 
quitKey = 'q'
nextPostKey = 'l' 
previousPostKey = 'k'

-- 2 subreddit/post/user search (Form)
stringInputForm :: Form String  (*)

-- 3 choose an element from the list (subreddit/post/comment) (Cursor)
    -- UIOutputs
-- 1 List of interactible elements (subreddits/posts)
-- 2 List of elements (comments)
-- 3 Images (embedded in posts)

tui :: IO()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState = TuiState --datastructure of all tui states
    {
    tuiStateGreeting :: [String]
    
    }
    deriving(Show, Eq)

-- Just a placeholder from the tutorial for convinience
    -- probably wouldnt need it 
type ResourceName = String 


-- the main App that conrols the resources 
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




