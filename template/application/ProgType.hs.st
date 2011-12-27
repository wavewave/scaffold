{-# LANGUAGE DeriveDataTypeable #-}

module $modulebase$.ProgType where 

import System.Console.CmdArgs

data $progtype$ = Test 
              deriving (Show,Data,Typeable)

test :: $progtype$
test = Test 

mode = modes [test]


