module Tui where

import Cursor.Simple.List.NonEmpty

import Brick.Main
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Forms

import Graphics.Vty.Input.Events

import Graphics.Image
import Graphics.Image.IO

import Prelude as P

import Network.Curl 

    -- UIInputs: user -> pc 
-- 1 Hotkeys 
quitKey = 'q'
nextPostKey = 'l' 
previousPostKey = 'k'

-- 2 subreddit/post/user search (Form)

-- 3 choose an element from the list (subreddit/post/comment) (Cursor)

srString :: String
srString = getLine

    -- UIOutputs
-- 1 List of interactible elements (subreddits/posts)
-- 2 List of elements (comments)
-- 3 Images (embedded in posts)


tui::IO ()
tui = do
    initialState <- buildInitialState  
    finalState <- defaultMain uiApp initialState
    print finalState

-- configuration on what to do 
uiApp :: App UIStates event_type String
uiApp =
    App { appDraw           = drawUI
        , appChooseCursor   = showcursorUI
        , appHandleEvent    = handleEvent
        , appStartEvent     = pure 
        , appAttrMap        = const $ attrMap mempty []--idk what to write here
        }

-- datastructure of UIStates 
data UIStates =
    UIStates { uiDefaultState :: [String]
          -- , uiInputState   :: NonEmptyCursor String -- do i even need this input state?
             , uiCursorState  :: NonEmptyCursor String
             , uiImageState   :: Image VU RGB Int
             }
             deriving(Show, Eq)

buildInitialState :: IO UIStates
buildInitialState = do
    pure UIStates {uiDefaultState = "test":[]}

-- definitions of all the functions from uiApp
drawUI :: UIStates -> [Widget String]
drawUI ts = [vBox $ P.map str $ uiDefaultState ts]

-- handles all events
handleEvent :: UIStates -> BrickEvent name event -> EventM name (Next UIStates)
handleEvent state event =
    case event of
      VtyEvent vtye ->
          case vtye of
            EvKey (KChar quitKey) [] -> halt state
            _-> continue state
      _-> continue state 


showcursorUI = showFirstCursor



--getSubredditSearchResults :: String -> [String]
--getSubredditSearchResults sr =  

split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

sToURL :: String -> URLString
sToURL sr = url 
    where
        url = "https://www.reddit.com/search/?q="++sr++"&type=sr" :: URLString

getSrHtml :: String -> String    
getSrHtml sr = snd pure (curlGetString (sToURL sr) [] )
