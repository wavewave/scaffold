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
                   base == 4.*, 
                   mtl == 2.*, 
                   cmdargs == 0.9.*, 
                   $projname$-client

Library
  hs-source-dirs: lib
  ghc-options: 	-Wall -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: 
                   base == 4.*, 
                   mtl == 2.*, 
                   directory == 1.1.*, 
                   filepath == 1.2.*,
                   cmdargs == 0.9.*, 
                   $projname$-type == 0.0.*,
                   configurator == 0.2.*,
                   bytestring == 0.9.*, 
                   http-enumerator == 0.7.*, 
                   http-types == 0.6.*, 
                   aeson == 0.4.*,
                   attoparsec == 0.10.*, 
                   time == 1.2.*, 
                   uuid == 1.2.*

  Exposed-Modules: 
                   $modulebase$.Client.ProgType
                   $modulebase$.Client.Job
                   $modulebase$.Client.Config
                   $modulebase$.Client.Command
  Other-Modules: 

 
		 