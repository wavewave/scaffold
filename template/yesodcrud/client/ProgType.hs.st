{-# LANGUAGE DeriveDataTypeable #-}

module $modulebase$.Client.ProgType where 

import System.FilePath
import System.Console.CmdArgs hiding (name)

data $projnameC$_client = Create { config :: FilePath, modulename :: String }
                  | Get    { config :: FilePath, name :: String } 
                  | Put    { config :: FilePath, name :: FilePath, modulename :: String } 
                  | Delete { config :: FilePath, name :: String } 
                  | GetList { config :: FilePath } 
              deriving (Show,Data,Typeable)

create :: $projnameC$_client
create = Create { config = "test.conf"
                , modulename = "" &= typ "MODULENAME" &= argPos 0
                }

get :: $projnameC$_client 
get = Get { config = "test.conf" 
          , name = "" &= typ "NAME" &= argPos 0 
          } 

put :: $projnameC$_client 
put = Put { config = "test.conf"
          , name = "" &= typ "NAME" &= argPos 0 
          , modulename = "" &= typ "NAME" &= argPos 1
          }

delete :: $projnameC$_client 
delete = Delete { config = "test.conf"
                , name = "" &= typ "NAME" &= argPos 0 
                }

getlist :: $projnameC$_client 
getlist = GetList { config = "test.conf" } 

mode = modes [ create, get, put, delete, getlist ]


