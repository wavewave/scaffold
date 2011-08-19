{-# LANGUAGE DeriveDataTypeable #-}

module $modulebase$.Type where 

import System.Console.CmdArgs

data $progtype$ = Test 
              deriving (Show,Data,Typeable)

test :: $progtype$
test = Test 

mode = modes [test]


