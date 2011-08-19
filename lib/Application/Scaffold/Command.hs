module Application.Scaffold.Command where

import Application.Scaffold.Type
import Application.Scaffold.Job

commandLineProcess :: Scaffold -> IO ()
commandLineProcess (Test conf) = do 
  putStrLn "test called"
  startJob conf


