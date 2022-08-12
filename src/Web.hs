

module Web where

import Prelude as P
import Control.Monad.IO.Class

import Network.Curl 
import Graphics.Vty.Input.Terminfo (functionKeyCapsTable)
import Graphics.Vty (initialAssumedState)

--getSubredditSearchResults :: String -> [String]
--getSubredditSearchResults sr =  

--sample
-- split the get request result
split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'


-- | Regex to use in the filter and split function
filterClass :: String
filterClass = "s@.*class=\"_2torGbn_fNOMbGw3UAasPl\">r/([^<]*)@\1@p"


-- | filter and split the String into List of tuples  (String, String) to later choose from 
filter :: IO String -> IO [(String, String)]
filter str = do
    (a,b) <- ({- the name of the subreddit -} , {- the link to the subreddit -})
    ls <- []:(a,b)
    return ls

-- | split the string into names


-- | split the string into links 


-- | build an address for the get request
sToURL :: String -> URLString
sToURL sr = url 
    where
        url = "https://www.reddit.com/search/?q="++sr++"&type=sr" :: URLString


-- | make a Get for a list of sr and save the result
getSrHtml :: String -> IO String    
getSrHtml sr = do
    (a, b) <- curlGetString (sToURL sr) []
    (return b) :: IO String
