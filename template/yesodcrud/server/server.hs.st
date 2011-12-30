{-# LANGUAGE OverloadedStrings #-}

module Main where

import $modulebase$.Server.Type
import $modulebase$.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "$projname$-server"
  acid <- openLocalState M.empty 
  warpDebug 7800 ($projnameC$Server acid)
