module Main where

import Graphics.Image
import Brick

data App state event resource_name = 
    {appDraw :: state -> [Widget resource_name]


someFunc = print "hello world"

main :: IO ()
main = someFunc
