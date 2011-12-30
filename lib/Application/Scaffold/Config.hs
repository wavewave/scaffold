{-# LANGUAGE OverloadedStrings #-}

module Application.Scaffold.Config where

-- import Text.Parsec
-- import HEP.Parser.Config
-- import Control.Monad.Identity
import Control.Monad 
import Control.Applicative 
import Data.Configurator.Types
import Data.Configurator as C

data NewAppConfig = NewAppConfig { 
  nac_projname :: String,
  nac_author :: String, 
  nac_year :: String,
  nac_licensetype :: String, 
  nac_modulebase :: String, 
  nac_executable :: String
} deriving Show 

data YesodCrudConfiguration = YesodCrudConfiguration { 
  ycc_projname :: String,
  ycc_author :: String, 
  ycc_year :: String, 
  ycc_licensetype :: String,
  ycc_modulebase :: String
}

 {-

configNewApp :: ParsecT String () Identity NewAppConfig
configNewApp = do 
  oneGroupFieldInput "newapp" $ 
    NewAppConfig <$> (oneFieldInput "projname")
                 <*> (oneFieldInput "author")
                 <*> (oneFieldInput "year")
                 <*> (oneFieldInput "licensetype")
                 <*> (oneFieldInput "modulebase")
                 <*> (oneFieldInput "executable")

-}

liftM6 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r ) 
       -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 
       -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = 
  do { x1 <- m1 ; x2 <- m2 ; x3 <- m3 ; x4 <- m4 ; x5 <- m5 ; x6 <- m6 ; return (f x1 x2 x3 x4 x5 x6 ) }



getYesodCrud :: FilePath -> IO (Maybe YesodCrudConfiguration)
getYesodCrud fp = do 
  config <- load [Required fp]
  liftM5 YesodCrudConfiguration 
    <$> C.lookup config "yesodcrud.projname" 
    <*> C.lookup config "yesodcrud.author"
    <*> C.lookup config "yesodcrud.year"
    <*> C.lookup config "yesodcrud.licensetype"
    <*> C.lookup config "yesodcrud.modulebase"

getNewApp :: FilePath -> IO (Maybe NewAppConfig)
getNewApp fp = do 
  config <- load [Required fp]
  liftM6 NewAppConfig
    <$> C.lookup config "newapp.projname" 
    <*> C.lookup config "newapp.author"
    <*> C.lookup config "newapp.year"
    <*> C.lookup config "newapp.licensetype"
    <*> C.lookup config "newapp.modulebase"
    <*> C.lookup config "newapp.executable"
