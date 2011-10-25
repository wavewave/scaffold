{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State

import Data.Enumerator.Util
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

histogramming :: (MonadIO m) => TH1D 
              -> E.Iteratee (Maybe (a,PtlInfoMap,[DecayTop PtlIDInfo])) m () 
histogramming hist = EL.foldM f () 
  where f () (Just (_,_,dtops)) = do  
          let matchmuon = matchDecayTopAndGet4Momentum (Terminal 13)
          let [Terminal muonmom]  = catMaybes (map matchmuon dtops)
              (_,eta_muon,_) = mom_2_pt_eta_phi muonmom 
          liftIO (fill1 hist (etatocosth eta_muon))
          return ()
        f () Nothing = return ()
 
main :: IO ()
main = do 
  let fp = "testtest_unweighted_events.lhe"
  tcanvas <- newTCanvas "TEST" "TEST" 640 480 
  h1 <- newTH1D "test" "test" 50 (-1.2) 1.2 
  let process = enumZip3 countIter countMarkerIter (histogramming h1)
  let iter = do 
         _ <- textLHEHeader
         parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process
  withFile fp ReadMode $ \ih -> runStateT (parseXmlFile ih iter) (0::Int)
  draw h1 ""
  saveAs tcanvas "test.pdf" "" 
  return ()
