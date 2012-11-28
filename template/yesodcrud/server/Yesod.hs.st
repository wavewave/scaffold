{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module $modulebase$.Server.Yesod where 

import           Control.Applicative
import           Data.Acid
import           Data.Aeson as A
import           Data.Attoparsec as P
import           Data.Conduit 
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as S
import           Data.UUID
import           Network.Wai
import           Yesod hiding (update)
--
import           $modulebase$.Type
import           $modulebase$.Server.Type


mkYesod "$projnameC$Server" [parseRoutes|
/ HomeR GET
/list$projname$  List$projnameC$R GET
/upload$projname$  Upload$projnameC$R POST
/$projname$/#UUID $projnameC$R 
|]

instance Yesod $projnameC$Server where
  -- approot _ = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage $projnameC$Server FormMessage where
  renderMessage _ _ = defaultFormMessage -}


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO \$ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]


defhlet :: GWidget s m ()
defhlet = [whamlet| <h1> HTML output not supported |]


getList$projnameC$R :: Handler RepHtmlJson
getList$projnameC$R = do 
  liftIO \$ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO \$ query acid QueryAll
  liftIO \$ putStrLn \$ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUpload$projnameC$R :: Handler RepHtmlJson
postUpload$projnameC$R = do 
  liftIO \$ putStrLn "postQueueR called" 
  acid <- return.server_acid =<< getYesod
  wr <- reqWaiRequest <\$> getRequest  
  bs' <- liftIO \$ runResourceT (requestBody wr \$\$ CL.consume)
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result $projnameC$Info) of 
        Success minfo -> do 
          r <- liftIO \$ update acid (Add$projnameC$ minfo)
          liftIO \$ print (Just r)
          liftIO \$ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO \$ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))
    Fail _ ctxts err -> do 
      liftIO \$ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))
    Partial _ -> do 
      liftIO \$ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))



handle$projnameC$R :: UUID -> Handler RepHtmlJson
handle$projnameC$R name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> get$projnameC$R name
    "PUT" -> put$projnameC$R name
    "DELETE" -> delete$projnameC$R name
    x -> error ("No such action " ++ show x ++ " in handler$projnameC$R")

get$projnameC$R :: UUID -> Handler RepHtmlJson
get$projnameC$R idee = do 
  liftIO \$ putStrLn "get$projnameC$R called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO \$ query acid (Query$projnameC$ idee)
  liftIO \$ putStrLn \$ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


put$projnameC$R :: UUID -> Handler RepHtmlJson
put$projnameC$R idee = do 
  liftIO \$ putStrLn "put$projnameC$R called"
  acid <- return.server_acid =<< getYesod
  wr <- reqWaiRequest <\$> getRequest  
  bs' <- liftIO \$ runResourceT (requestBody wr \$\$ CL.consume)
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO \$ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result $projnameC$Info) of 
        Success minfo -> do 
          if idee == $projname$_uuid minfo
            then do r <- liftIO \$ update acid (Update$projnameC$ minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO \$ putStrLn "$projname$name mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))
        Error err -> do 
          liftIO \$ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))
    Fail _ ctxts err -> do 
      liftIO \$ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))
         
    Partial _ -> do 
      liftIO \$ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe $projnameC$Info))

delete$projnameC$R :: UUID -> Handler RepHtmlJson
delete$projnameC$R idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO \$ update acid (Delete$projnameC$ idee)
  liftIO \$ putStrLn \$ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))

