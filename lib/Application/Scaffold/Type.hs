{-# LANGUAGE DeriveDataTypeable #-}

module Application.Scaffold.Type where 

import System.Console.CmdArgs

data Scaffold = Test { 
                config :: FilePath
              } 
              deriving (Show,Data,Typeable)

test :: Scaffold
test = Test { config = "test.conf" 
            } 

mode = modes [test]
