module Main where

import System.Console.CmdArgs

import $modulebase$.Client.ProgType
import $modulebase$.Client.Command

main :: IO () 
main = do 
  putStrLn "$projname$-client"
  param <- cmdArgs mode
  commandLineProcess param