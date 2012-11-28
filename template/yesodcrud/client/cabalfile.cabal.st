Name:		$projname$-client
Version:	0.0
Synopsis:	client program that communicates with model-server
Description: 	client program that communicates with model-server
License: 	$licensetype$
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Build-Type: 	Simple
Cabal-Version:  >= 1.8
data-files:     


Executable $projname$-client
  Main-is: $projname$-client.hs
  hs-source-dirs: exe
  ghc-options: 	-Wall -O2 -threaded -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: 
                   base, 
                   mtl, 
                   cmdargs, 
                   $projname$-client

Library
  hs-source-dirs: lib
  ghc-options: 	-Wall -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: 
                   base, 
                   mtl, 
                   directory, 
                   filepath,
                   cmdargs, 
                   $projname$-type,
                   configurator,
                   bytestring, 
                   http-conduit, 
                   http-types, 
                   aeson,
                   attoparsec, 
                   time, 
                   uuid

  Exposed-Modules: 
                   $modulebase$.Client.ProgType
                   $modulebase$.Client.Job
                   $modulebase$.Client.Config
                   $modulebase$.Client.Command
  Other-Modules: 

 
		 