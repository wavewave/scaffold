module Main where

import System.Console.CmdArgs

import $modulebase$.ProgType
import $modulebase$.Command

main :: IO () 
main = do 
  putStrLn "$projname$"
  param <- cmdArgs mode

  commandLineProcess param
