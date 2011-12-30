module Application.Scaffold.Generate.YesodCrud where

import Application.DevAdmin.Config
import Application.Scaffold.Config


import Paths_scaffold

createYesodCrud :: BuildConfiguration -> YesodCrudConfiguration -> IO ()
createYesodCrud bc ycc = do 
  putStrLn "inside createYesodCrud"
