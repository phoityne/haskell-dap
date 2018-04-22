module Haskell.DAP.GHCi.Type where

import qualified GHC
import RtClosureInspect

import qualified Data.Map as M


-- |
--
type EvalString = String


-- |
--
data DAPContext = DAPContext {
    variableReferenceMapDAPContext :: M.Map Int (Term, EvalString)
  , bindingDAPContext :: [GHC.TyThing]
  , frameIdDAPContext :: Int
  , srcBPsDAPContext  :: M.Map Int FilePath
  , funcBPsDAPContext :: [Int]
  }

  
-- |
--
defaultDAPContext :: DAPContext
defaultDAPContext = DAPContext {
    variableReferenceMapDAPContext = M.fromList []
  , bindingDAPContext = []
  , frameIdDAPContext = 0
  , srcBPsDAPContext  = M.fromList []
  , funcBPsDAPContext = []
  }

  