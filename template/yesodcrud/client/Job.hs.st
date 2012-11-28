{-# LANGUAGE OverloadedStrings #-}

module $modulebase$.Client.Job where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import qualified Data.Attoparsec as A

import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.HTTP.Types.Status

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import $modulebase$.Client.Config
import $modulebase$.Type
import Data.UUID
import Data.UUID.V5
import qualified Data.ByteString as B
import Data.Time.Clock

type Url = String 

nextUUID :: $projnameC$ClientConfiguration -> IO UUID
nextUUID mc = do 
  let c = $projname$ClientURL mc 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack \$ c ++ "/" ++ show t 

startCreate :: $projnameC$ClientConfiguration -> String -> IO () 
startCreate mc name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = $projname$ServerURL mc 
  uuid <- nextUUID mc
  let info = $projnameC$Info { $projname$_uuid = uuid , $projname$_name = name } 
  response <- $projname$ToServer url ("upload$projname$") methodPost info
  putStrLn \$ show response 


startGet :: $projnameC$ClientConfiguration -> String -> IO () 
startGet mc idee = do 
  putStrLn \$"get " ++ idee
  let url = $projname$ServerURL mc 
  r <- jsonFromServer url ("$projname$" </> idee) methodGet
  putStrLn \$ show r 


startPut :: $projnameC$ClientConfiguration 
         -> String  -- ^ $projname$ idee
         -> String  -- ^ $projname$ name 
         -> IO () 
startPut mc idee name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = $projname$ServerURL mc 
      info = case fromString idee of 
               Nothing -> error "strange in startPut" 
               Just idee' -> $projnameC$Info { $projname$_uuid = idee', $projname$_name = name }
  response <- $projname$ToServer url ("$projname$" </> idee) methodPut info
  putStrLn \$ show response 


startDelete :: $projnameC$ClientConfiguration -> String -> IO () 
startDelete mc idee = do 
  putStrLn "job started"
  let url = $projname$ServerURL mc 
  r <- jsonFromServer url ("$projname$" </> idee) methodDelete
  putStrLn \$ show r 


startGetList :: $projnameC$ClientConfiguration -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = $projname$ServerURL mc 
  r <- jsonFromServer url ("list$projname$") methodGet
  putStrLn \$ show r 


jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager \$ \\manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody \$ r
      else return (Left \$ "status code : " ++ show (statusCode (responseStatus r))) 

$projname$ToServer :: Url -> String -> Method -> $projnameC$Info -> IO (Either String (Result Value))
$projname$ToServer url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager \$ \\manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody \$ r
      else return (Left \$ "status code : " ++ show (statusCode (responseStatus r))) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) \$ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 
