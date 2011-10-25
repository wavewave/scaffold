{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State

import Data.Enumerator hiding (map,length,break,head)
import qualified Data.Enumerator as E (Iteratee)
import qualified Data.Enumerator.List as EL 
import Data.Maybe
import Data.Enumerator.Util.Count 

import Text.XML.Enumerator.Parse.Util

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Parser.Enumerator
import HEP.Parser.LHEParser.DecayTop
import HEP.Util.Functions

import HROOT

import System.IO

histogramming :: (MonadIO m) => TH1D -> E.Iteratee (Maybe (a,PtlInfoMap,[DecayTop PtlIDInfo])) m () 
histogramming hist = EL.foldM f ()
  where f () Nothing = return () 
        f () (Just (_,_,dtops)) = do 
          let terms = Prelude.concatMap (flip findonlyTerminal []) dtops
              match (Terminal pinfo) = if pdgid pinfo == (-11) 
                                          || pdgid pinfo == (-13) 
                                       then Just (ptlinfo pinfo)
                                       else Nothing
              match _ = Nothing 
              lst = mapMaybe (\x -> match x >>= (return . mom_2_pt_eta_phi . pupTo4mom . pup ))  terms 
              histfunc (ptlep,etalep,_) = when (ptlep > 50) $ do {fill1 hist (etatocosth etalep); return ()}
          mapM_ (liftIO . histfunc) lst 
         
 
main :: IO ()
main = do 
  let fp = "uubarttbarsemilep_300.lhe"
  tcanvas <- newTCanvas "TEST" "TEST" 640 480 
  h1 <- newTH1D "test" "test" 50 (-1.2) 1.2 
  let process = EL.zip3 countIter countMarkerIter (histogramming h1)
  let iter = do 
         _ <- textLHEHeader
         parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process
  withFile fp ReadMode $ \ih -> runStateT (parseXmlFile ih iter) (0::Int)
  draw h1 ""
  saveAs tcanvas "test.pdf" "" 
  return ()
