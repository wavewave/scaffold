module Application.Scaffold.Generate.NewApp where

import System.Directory
import System.FilePath
-- import System.Environment
import System.Process
-- import System.IO
import System.Exit

-- import Text.Parsec
import Text.StringTemplate
import Text.StringTemplate.Helpers

import Control.Monad
import Data.Char
import Data.List.Split

import Application.DevAdmin.Config
import Application.Scaffold.Config
import Application.Scaffold.Generate.Darcs 

import Paths_scaffold

createNewApp :: BuildConfiguration -> NewAppConfig -> IO () 
createNewApp bc nac = do
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
  let exposedmodules =    "                   " ++ mbase ++ ".ProgType\n"
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
  mapM_ mkhsfile [ "ProgType.hs", "Job.hs", "Command.hs" ]
  setCurrentDirectory basedir 
  createDirectory "exe"
  setCurrentDirectory "exe"
  let appstr = renderTemplateGroup apptmpl replacement "project.hs"
  writeFile (executable++".hs") appstr
  setCurrentDirectory basedir 
  let allfiles = [ "LICENSE", "Setup.lhs", "lib", "exe", (projname++".cabal")] 
                 ++ map ("lib" </>) moduledirs  
                 ++ ["lib" </> (last moduledirs) </> "ProgType.hs"]
                 ++ ["lib" </> (last moduledirs) </> "Job.hs"]
                 ++ ["lib" </> (last moduledirs) </> "Command.hs"]
                 ++ [ "exe" </> executable ++ ".hs" ]
  darcsInit  
  mapM_ darcsFile allfiles 
  darcsRecord ("initialize " ++ projname)
  return () 


