{-# LANGUAGE OverloadedStrings #-}

module $modulebase$.Server.Type where

import Control.Applicative
import Data.Text.Encoding as E
import Data.UUID
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Yesod.Dispatch
import Text.Blaze
import $modulebase$.Type
-- import Debug.Trace 
import Data.Acid

instance PathPiece UUID where
  fromPathPiece = fromString . C.unpack . E.encodeUtf8
  toPathPiece = E.decodeUtf8 . C.pack . toString 

instance ToMarkup UUID where
  toMarkup = toMarkup . toString 

data $projnameC$Server = $projnameC$Server {
  server_acid :: AcidState $projnameC$InfoRepository
}

