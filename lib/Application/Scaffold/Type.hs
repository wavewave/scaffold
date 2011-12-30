{-# LANGUAGE DeriveDataTypeable #-}

module Application.Scaffold.Type where 

import System.Console.CmdArgs

data Scaffold = MakeApp { config :: FilePath }
              | MakeYesodCrud { config :: FilePath }
              deriving (Show,Data,Typeable)

makeapp :: Scaffold
makeapp = MakeApp { config = "test.conf"  } 

makeyesodcrud :: Scaffold
makeyesodcrud = MakeYesodCrud { config = "test.conf" } 

mode :: Scaffold
mode = modes [ makeapp, makeyesodcrud ]
