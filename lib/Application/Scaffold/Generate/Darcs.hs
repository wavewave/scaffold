module Application.Scaffold.Generate.Darcs where

import System.Process
import System.Exit

darcsInit :: IO ExitCode
darcsInit = system "darcs init"

darcsFile :: FilePath -> IO ExitCode 
darcsFile fp = do 
  putStrLn $ "add " ++ fp
  system ("darcs add " ++ fp)

darcsRecord :: String -> IO ExitCode
darcsRecord patchname = 
  system ("darcs record --all -m \"" ++ patchname ++ "\"")
 
