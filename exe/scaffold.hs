module Main where

import System.Console.CmdArgs

import Application.Scaffold.Type
import Application.Scaffold.Command

main :: IO () 
main = do 
  putStrLn "scaffold"
  param <- cmdArgs mode

  commandLineProcess param


