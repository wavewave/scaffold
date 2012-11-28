Name:		$projname$-server
Version:	0.0
Synopsis:	CRUD sample server
Description: 	CRUD sample server
License: 	$licensetype$
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Build-Type: 	Simple
Cabal-Version:  >= 1.8

Executable $projname$-server
  Main-is: $projname$-server.hs
  hs-source-dirs: exe
  Build-Depends: base, 
                 yesod, 
                 uuid,
                 bytestring, 
                 containers, 
                 acid-state, 
                 $projname$-type,
                 $projname$-server
  ghc-options: 	-Wall -O2 -threaded -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all

Library
  hs-source-dirs: lib
  ghc-options: 	-Wall -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: base, 
                 containers, 
                 mtl, 
                 yesod, 
                 $projname$-type, 
                 safecopy, 
                 acid-state, 
                 aeson, 
                 bytestring, 
                 blaze-markup,
                 conduit, 
                 uuid,
                 wai-extra, 
                 wai,
                 attoparsec, 
                 text, 
                 yesod-core, 
                 yesod-form, 
                 blaze-html
  Exposed-Modules: 
                   $modulebase$.Server.Type
                   $modulebase$.Server.Yesod



		 
