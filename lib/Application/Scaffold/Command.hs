module Application.Scaffold.Command where

import Application.Scaffold.Type
import Application.Scaffold.Job

commandLineProcess :: Scaffold -> IO ()
commandLineProcess (MakeApp conf) = do 
  putStrLn "makeapp called"
  startMakeApp conf
commandLineProcess (MakeYesodCrud conf) = do 
  putStrLn "makeyesodcrud called" 
  startMakeYesodCrud conf 

