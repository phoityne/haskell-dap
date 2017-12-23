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
    variableReferenceMapDAPContext :: M.Map String (Term, EvalString)
  , bindingDAPContext :: [GHC.TyThing]
  , frameIdDAPContext :: Int
  }

  
-- |
--
defaultDAPContext :: DAPContext
defaultDAPContext = DAPContext {
    variableReferenceMapDAPContext = M.fromList []
  , bindingDAPContext = []
  , frameIdDAPContext = 0
  }

  