module Application.Scaffold.Job where

import Application.DevAdmin.Config
import Application.Scaffold.Config

import System.Directory
import System.FilePath
import System.Environment
import System.Process
import System.IO
import System.Exit

import Text.Parsec
import Text.StringTemplate
import Text.StringTemplate.Helpers

import Control.Monad
import Data.Char
import Data.List.Split

import Paths_scaffold

startJob :: FilePath -> IO () 
startJob configfile = do 
  putStrLn "job started"
  putStrLn "reading .build"
  homedir <- getEnv "HOME"
  buildconfigstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" buildconfigstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do 
      putStrLn $ show bc 
      projconfstr <- readFile configfile 
      let proj_result = parse configNewApp "" projconfstr 
      case proj_result of 
        Left err -> putStrLn (show err)
        Right nac -> do 
          putStrLn $ show nac 
          createApp bc nac 

createApp :: BuildConfiguration -> NewAppConfig -> IO () 
createApp bc nac = do
  let projname = nac_projname nac
      licensetype = nac_licensetype nac
  putStrLn $ "creating " ++ projname
  createDirectory projname
  setCurrentDirectory projname 

  basedir <- getCurrentDirectory

  datadir <- getDataDir
  let tmpldir = datadir </> "template"
  let licensedir = tmpldir </> "licenses"
  let licensefilename = case licensetype of 
                          "BSD3" -> "LICENSE-BSD3"
                          "GPL-3" -> "LICENSE-GPL-3"
                          "MIT" -> "LICENSES-MIT"
                          _ -> error "no such license type"
  licensetmpl <- directoryGroup licensedir 
  let licensestr = renderTemplateGroup
                     licensetmpl 
                     [ ("name", nac_author nac)
                     , ("year", nac_year nac) ] 
                     licensefilename
                     
  writeFile "LICENSE" licensestr
  
  copyFile (tmpldir </> "Setup.lhs") "Setup.lhs" 

  let cabaldir = tmpldir </> "cabal"
  cabaltmpl <- directoryGroup cabaldir 
  let mbase = nac_modulebase nac
  let exposedmodules =    "                   " ++ mbase ++ ".Type\n"
                       ++ "                   " ++ mbase ++ ".Job\n"
                       ++ "                   " ++ mbase ++ ".Command"
  let libdep =    "                   base>4, mtl>2, directory, filepath,\n"
               ++ "                   cmdargs"
      exedep =    "                   base>4, mtl>2, directory, filepath,\n"
               ++ "                   cmdargs, " ++ projname
  let progtype = let (c:cs) = projname
                     c' = toUpper c
                     cs' = map (\x->if x=='-' then '_' else x) cs  
                 in (c':cs')
      executable = map toLower projname 
  let replacement = [ ("projname",projname)
                    , ("licensetype", licensetype ) 
                    , ("executable", executable )
                    , ("libdep", libdep) 
                    , ("exedep", exedep)
                    , ("exposedmodules", exposedmodules)
                    , ("modulebase", mbase)
                    , ("progtype", progtype) 
                    ] 

  let cabalstr = renderTemplateGroup 
                   cabaltmpl
                   replacement 
                   "project.cabal"
  writeFile (projname++".cabal") cabalstr 

  createDirectory "lib"
  setCurrentDirectory "lib"
 
  let splitted = splitOn "." mbase 
  forM_ splitted $ \x -> do 
    createDirectory x 
    setCurrentDirectory x
  let moduledirs = filter (not.null) (scanl (</>) [] splitted)
    

  putStrLn $ show moduledirs   


  let appdir = tmpldir  </> "application"
  apptmpl <- directoryGroup appdir 
  let mkhsfile x = do 
        let str = renderTemplateGroup apptmpl replacement x
        writeFile x str 
       
  mapM_ mkhsfile [ "Type.hs", "Job.hs", "Command.hs" ]

  setCurrentDirectory basedir 
  createDirectory "exe"
  setCurrentDirectory "exe"
  let appstr = renderTemplateGroup apptmpl replacement "project.hs"
  writeFile (executable++".hs") appstr

  setCurrentDirectory basedir 
  let allfiles = [ "LICENSE", "Setup.lhs", "lib", "exe", (projname++".cabal")] 
                 ++ map ("lib" </>) moduledirs  
                 ++ ["lib" </> (last moduledirs) </> "Type.hs"]
                 ++ ["lib" </> (last moduledirs) </> "Job.hs"]
                 ++ ["lib" </> (last moduledirs) </> "Command.hs"]
                 ++ [ "exe", "exe" </> executable ++ ".hs" ]
  darcsInit  
  mapM_ darcsFile allfiles 
  darcsRecord ("initialize " ++ projname)

  return () 

darcsInit :: IO ExitCode
darcsInit = system "darcs init"

darcsFile :: FilePath -> IO ExitCode 
darcsFile fp = do 
  putStrLn $ "add " ++ fp
  system ("darcs add " ++ fp)

darcsRecord :: String -> IO ExitCode
darcsRecord patchname = 
  system ("darcs record --all -m \"" ++ patchname ++ "\"")
 




