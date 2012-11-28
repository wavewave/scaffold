{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings, 
             FlexibleInstances  #-}

module $modulebase$.Type where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.SafeCopy
import qualified Data.Map as M

import Data.Acid 
import Data.UUID
import Data.Aeson
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B

data $projnameC$Info = $projnameC$Info { 
  $projname$_uuid :: UUID, 
  $projname$_name :: String
} deriving (Show,Typeable,Data)


instance FromJSON UUID where
  parseJSON x = do r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
                   case r of 
                     Nothing -> fail ("UUID parsing failed " ++ show x )
                     Just uuid -> return uuid 

instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON $projnameC$Info where
  parseJSON (Object v) = $projnameC$Info <\$>  v .: "uuid" <*> v .: "name"

instance ToJSON $projnameC$Info where
  toJSON ($projnameC$Info uuid name) = object [ "uuid" .= uuid , "name" .= name ] 


instance SafeCopy UUID where 
  putCopy uuid = contain \$ safePut (toByteString uuid) 
  getCopy = contain 
            \$ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

\$(deriveSafeCopy 0 'base ''$projnameC$Info)

type $projnameC$InfoRepository = M.Map UUID $projnameC$Info 

add$projnameC$ :: $projnameC$Info -> Update $projnameC$InfoRepository $projnameC$Info 
add$projnameC$ minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\\_k _o n -> n) ($projname$_uuid minfo) minfo m
  put m'
  return minfo
 
query$projnameC$ :: UUID -> Query $projnameC$InfoRepository (Maybe $projnameC$Info) 
query$projnameC$ uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query $projnameC$InfoRepository [$projnameC$Info]
queryAll = do m <- ask   
              return (M.elems m)


update$projnameC$ :: $projnameC$Info -> Update $projnameC$InfoRepository (Maybe $projnameC$Info)
update$projnameC$ minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\\_ _ -> Just minfo) ($projname$_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

delete$projnameC$ :: UUID -> Update $projnameC$InfoRepository (Maybe $projnameC$Info)
delete$projnameC$ uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


\$(makeAcidic ''$projnameC$InfoRepository [ 'add$projnameC$, 'query$projnameC$, 'queryAll, 'update$projnameC$, 'delete$projnameC$] )

