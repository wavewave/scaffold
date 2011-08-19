module $modulebase$.Command where

import $modulebase$.Type
import $modulebase$.Job

commandLineProcess :: $progtype$ -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob

