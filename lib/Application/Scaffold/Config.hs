module Application.Scaffold.Config where

import Text.Parsec

import HEP.Parser.Config
import Control.Monad.Identity
import Control.Applicative 

data NewAppConfig = NewAppConfig { 
  nac_projname :: String,
  nac_author :: String, 
  nac_year :: String,
  nac_licensetype :: String, 
  nac_modulebase :: String, 
  nac_executable :: String
} deriving Show 

configNewApp :: ParsecT String () Identity NewAppConfig
configNewApp = do 
  oneGroupFieldInput "newapp" $ 
    NewAppConfig <$> (oneFieldInput "projname")
                 <*> (oneFieldInput "author")
                 <*> (oneFieldInput "year")
                 <*> (oneFieldInput "licensetype")
                 <*> (oneFieldInput "modulebase")
                 <*> (oneFieldInput "executable")

