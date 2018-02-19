module Haskell.DAP.GHCi.Type where

import qualified GHC
import RtClosureInspect

import qualified Data.Map as M


-- |
--
type EvalString = String


-- |
--
data BreakpointType = SourceBreakpoint | FunctionBreakpoint
  deriving (Show, Read, Eq)

-- |
--
type BreakpointTypeMap = M.Map Int BreakpointType


-- |
--
data DAPContext = DAPContext {
    variableReferenceMapDAPContext :: M.Map Int (Term, EvalString)
  , bindingDAPContext :: [GHC.TyThing]
  , frameIdDAPContext :: Int
  , bpTypeMapDAPContext :: BreakpointTypeMap
  }

  
-- |
--
defaultDAPContext :: DAPContext
defaultDAPContext = DAPContext {
    variableReferenceMapDAPContext = M.fromList []
  , bindingDAPContext = []
  , frameIdDAPContext = 0
  , bpTypeMapDAPContext = M.fromList []
  }

  