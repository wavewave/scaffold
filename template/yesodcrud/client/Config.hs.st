{-# LANGUAGE OverloadedStrings #-}

module $modulebase$.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data $projnameC$ClientConfiguration = $projnameC$ClientConfiguration { 
  $projname$ServerURL :: String,
  $projname$ClientURL :: String
} deriving (Show)

get$projnameC$ClientConfiguration :: Config -> IO (Maybe $projnameC$ClientConfiguration)
get$projnameC$ClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  ($projnameC$ClientConfiguration  <\$> s <*> c )

