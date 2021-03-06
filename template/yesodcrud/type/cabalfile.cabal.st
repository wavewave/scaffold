Name:		$projname$-type
Version:	0.0
Synopsis:	CRUD sample type 
Description: 	CRUD sample type 
License: 	$licensetype$
License-file:	LICENSE
Author:		Ian-Woo Kim
Maintainer: 	Ian-Woo Kim <ianwookim@gmail.com>
Build-Type: 	Simple
Cabal-Version:  >= 1.8
data-files:     

Library
  hs-source-dirs: lib
  ghc-options: 	-Wall -funbox-strict-fields -fno-warn-unused-do-bind
  ghc-prof-options: -caf-all -auto-all
  Build-Depends: 
                   base, 
                   mtl, 
                   uuid,
                   containers, 
                   safecopy, 
                   acid-state, 
                   bytestring, 
                   text, 
                   aeson
  exposed-modules: $modulebase$.Type
