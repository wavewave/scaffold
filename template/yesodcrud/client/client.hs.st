module Main where

import System.Console.CmdArgs

import $projnameC$.Client.ProgType
import $projnameC$.Client.Command

main :: IO () 
main = do 
  putStrLn "$projname$-client"
  param <- cmdArgs mode
  commandLineProcess param