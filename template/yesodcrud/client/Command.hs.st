module $modulebase$.Client.Command where

import $modulebase$.Client.ProgType
import $modulebase$.Client.Job
import $modulebase$.Client.Config
import Data.Configurator

commandLineProcess :: $projnameC$_client -> IO ()
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  mc <- get$projnameC$ClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startCreate mn) mc
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- get$projnameC$ClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startGet n) mc
commandLineProcess (Put cfg n mn) = do 
  putStrLn "put called"
  mc <- get$projnameC$ClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\\c-> startPut c n mn) mc
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  mc <- get$projnameC$ClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startDelete n) mc
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  mc <- get$projnameC$ClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") startGetList mc
