{-# LANGUAGE ScopedTypeVariables #-}

module Application.Scaffold.Generate.YesodCrud where

import System.Directory 
import System.FilePath
import Control.Monad
--import Data.List.Split
import Data.Char

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Application.DevAdmin.Config
import Application.Scaffold.Config

import Paths_scaffold

createYesodCrud :: BuildConfiguration -> YesodCrudConfiguration -> IO ()
createYesodCrud bc ycc = do 
  putStrLn "inside createYesodCrud"
  let projname = ycc_projname ycc 
      licensetype = ycc_licensetype ycc
      projnametype = projname ++ "-type"
      projnameserver = projname ++ "-server"
      projnameclient = projname ++ "-client"
  putStrLn $ "creating " ++ projname 
  -- createDirectory projnametype
  -- createDirectory projnameserver
  -- createDirectory projnameclient 

  cdir <- getCurrentDirectory 
  let pathtype = cdir </> projnametype
      pathserver = cdir </> projnameserver 
      pathclient = cdir </> projnameclient 

  let mbase = ycc_modulebase ycc 

  -- makeDirectoriesForModules ycc YSType pathtype 
  makeDirectoriesForModules ycc YSServer pathserver 
  -- makeDirectoriesForModules ycc YSClient pathclient 


data YesodSubPackageType = YSType | YSServer | YSClient 

makeDirectoriesForModules :: YesodCrudConfiguration -> YesodSubPackageType -> FilePath -> IO () 
makeDirectoriesForModules ycc stype fp = do 
{-  setCurrentDirectory fp 
  createDirectory "lib"
  setCurrentDirectory "lib"
  let mbase = ycc_modulebase ycc 
  let splitted = splitOn "." mbase 
  forM_ splitted $ \x -> do 
    createDirectory x 
    setCurrentDirectory x
  let moduledirs = filter (not.null) (scanl (</>) [] splitted)
  putStrLn $ show moduledirs   
  setCurrentDirectory fp 
  createDirectory "exe" -}
  
  let projname = ycc_projname ycc
      mbase = ycc_modulebase ycc 


  yesodcrudtmpl <- getDataDir >>= return . (</> "template" </> "yesodcrud")

  let tmpldir = case stype of 
                  YSType -> yesodcrudtmpl </> "type"
                  YSServer -> yesodcrudtmpl </> "server"
                  YSClient -> yesodcrudtmpl </> "client"
  (tmplgroup :: STGroup String) <- directoryGroup tmpldir 
  let cabalstr = renderTemplateGroup 
                   tmplgroup 
                    [ ("projname" , projname)
                    , ("modulebase", mbase) ] 
                    "cabalfile.cabal"

   --  putStrLn cabalstr 

  case stype of 
    YSType -> do 
      let str = renderTemplateGroup 
                  tmplgroup 
                  [ ("projname" , projname) 
                  , ("modulebase", mbase)
                  , ("projnameC", capital1st projname) ] 
                  "Type.hs"
      putStrLn str 
    YSClient -> do 
      let str = renderTemplateGroup 
                  tmplgroup 
                  [ ("projname" , projname) 
                  , ("modulebase", mbase)
                  , ("projnameC", capital1st projname) ] 
                  "client.hs"
      putStrLn str 
    YSServer -> do 
      let str = renderTemplateGroup 
                  tmplgroup 
                  [ ("projname" , projname) 
                  , ("modulebase", mbase)
                  , ("projnameC", capital1st projname) ] 
                  "server.hs"
      putStrLn str



capital1st :: String -> String 
capital1st [] = [] 
capital1st (x:xs) = toUpper x : xs 
