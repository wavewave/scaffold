module Application.Scaffold.Job where

import Application.DevAdmin.Config
import Application.DevAdmin.ProgType
import Application.Scaffold.Config

import Application.Scaffold.Generate.NewApp
import Application.Scaffold.Generate.YesodCrud

startMakeYesodCrud :: FilePath -> IO () 
startMakeYesodCrud configfile = do 
    b <- constructBuildModes 
    putStrLn "startMakeYesodCrud"
    withBuildFile b $ \(bc,pc) -> 
      getYesodCrud configfile >>= maybe (putStrLn "parsing config file error") (createYesodCrud bc)

startMakeApp :: FilePath -> IO () 
startMakeApp configfile = do 
     b <- constructBuildModes 
     withBuildFile b $ \(bc,pc) -> 
       getNewApp configfile >>=  maybe (putStrLn "parsing config file error") (createNewApp bc) 




