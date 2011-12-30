{-# LANGUAGE ScopedTypeVariables #-}

module Application.Scaffold.Generate.YesodCrud where

import System.Directory 
import System.FilePath
import Control.Monad
import Data.List.Split
import Data.Char

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Application.DevAdmin.Config
import Application.Scaffold.Config
import Application.Scaffold.Generate.Darcs

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
  createDirectory projnametype
  createDirectory projnameserver
  createDirectory projnameclient 

  cdir <- getCurrentDirectory 
  let pathtype = cdir </> projnametype
      pathserver = cdir </> projnameserver 
      pathclient = cdir </> projnameclient 

  let mbase = ycc_modulebase ycc 

  makeDirectoriesAndFiles ycc YSType pathtype 
  makeDirectoriesAndFiles ycc YSServer pathserver 
  makeDirectoriesAndFiles ycc YSClient pathclient 


data YesodSubPackageType = YSType | YSServer | YSClient 

makeDirectoriesAndFiles :: YesodCrudConfiguration -> YesodSubPackageType -> FilePath -> IO () 
makeDirectoriesAndFiles ycc stype fp = do 
    setCurrentDirectory fp 
    createDirectory "lib"
    setCurrentDirectory "lib"
    let mbase = ycc_modulebase ycc 
    let splitted = splitOn "." mbase 
    forM_ splitted $ \x -> do 
      createDirectory x 
      setCurrentDirectory x
    let moduledir = foldl (</>) [] splitted
           -- filter (not.null) (scanl (</>) [] splitted)
    putStrLn $ "moduledir = " ++ moduledir   
    case stype of 
      YSServer -> createDirectory (fp </> "lib" </> moduledir </> "Server")
      YSClient -> createDirectory (fp </> "lib" </> moduledir </> "Client")
      _ -> return ()

    case stype of 
      YSServer -> createDirectory (fp </> "exe")
      YSClient -> createDirectory (fp </> "exe")
      _ -> return ()
     
    setCurrentDirectory fp 
    datadir <- getDataDir 
    let projname = ycc_projname ycc
        mbase = ycc_modulebase ycc 
        licensetype = ycc_licensetype ycc
        templatebasedir = datadir </> "template"
        licensedir = datadir </> "template" </> "licenses"
        licensefilename = case licensetype of 
                            "BSD3" -> "LICENSE-BSD3"
                            "GPL-3" -> "LICENSE-GPL-3"
                            "MIT" -> "LICENSES-MIT"
                            _ -> error "no such license type"
    licensetmpl <- directoryGroup licensedir
    let licensestr = renderTemplateGroup 
                       licensetmpl 
                       [ ("name", ycc_author ycc)
                       , ("year", ycc_year ycc) ] 
                       licensefilename
    writeFile "LICENSE" licensestr 
    copyFile (templatebasedir </> "Setup.lhs") "Setup.lhs"
 
    let yesodcrudtmpl = datadir </> "template" </> "yesodcrud"
    let tmpldir = case stype of 
                    YSType -> yesodcrudtmpl </> "type"
                    YSServer -> yesodcrudtmpl </> "server"
                    YSClient -> yesodcrudtmpl </> "client"
    (tmplgroup :: STGroup String) <- directoryGroup tmpldir 
    let cabalstr = renderTemplateGroup 
                     tmplgroup 
                      [ ("projname" , projname)
                      , ("modulebase", mbase) 
                      , ("licensetype", licensetype) ] 
                      "cabalfile.cabal"
    case stype of 
      YSType -> writeFile (projname++ "-type.cabal") cabalstr 
      YSClient -> writeFile (projname ++ "-client.cabal") cabalstr
      YSServer -> writeFile (projname ++ "-server.cabal") cabalstr 

    let makestr tmplgrp = renderTemplateGroup 
                            tmplgroup 
                            [ ("projname" , projname) 
                            , ("modulebase", mbase)
                            , ("projnameC", capital1st projname) ]
    setCurrentDirectory fp 
    case stype of 
      YSType -> do 
        let strType = makestr tmplgroup "Type.hs"
        writeFile ("lib" </> moduledir </> "Type.hs") strType
      YSClient -> do 
        let [strClient,strCommand,strConfig,strJob,strProgType] = 
              map (makestr tmplgroup) [ "client.hs", "Command.hs", "Config.hs", "Job.hs", "ProgType.hs" ]
        mapM_ (\(x, y) -> writeFile ("lib" </> moduledir </> "Client" </> x ) y ) 
              [ ("Command.hs", strCommand)
              , ("Config.hs", strConfig)
              , ("Job.hs", strJob)
              , ("ProgType.hs",strProgType) ] 
        writeFile ("exe" </> projname ++ "-client.hs") strClient
      YSServer -> do 
        let [strType, strYesod, strServer] = 
              map (makestr tmplgroup) [ "Type.hs", "Yesod.hs", "server.hs" ] 
        mapM_ (\(x, y) -> writeFile ("lib" </> moduledir </> "Server" </> x ) y ) 
              [ ("Type.hs", strType)
              , ("Yesod.hs", strYesod) ]
        writeFile ("exe" </> projname ++ "-server.hs") strServer
    let allfiles = [ "LICENSE", "Setup.lhs" ] ++ 
                   case stype of 
                     YSType -> [ projname++"-type.cabal", "lib" </> moduledir </> "Type.hs" ] 
                     YSClient -> [ projname++"-client.cabal"
                                 , "lib" </> moduledir </> "Client" </> "Command.hs"
                                 , "lib" </> moduledir </> "Client" </> "Config.hs"
                                 , "lib" </> moduledir </> "Client" </> "Job.hs"
                                 , "lib" </> moduledir </> "Client" </> "ProgType.hs"
                                 , "exe" </> projname++"-client.hs" ] 
                     YSServer -> [ projname++"-server.cabal"
                                 , "lib" </> moduledir </> "Server" </> "Type.hs"
                                 , "lib" </> moduledir </> "Server" </> "Yesod.hs" 
                                 , "exe" </> projname++"-server.hs" ]
    darcsInit
    mapM_ darcsFile allfiles 
    darcsRecord ("initialize " ++ projname) 
    return ()

capital1st :: String -> String 
capital1st [] = [] 
capital1st (x:xs) = toUpper x : xs 
