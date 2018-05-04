module Haskell.DAP.GHCi.Type where

import qualified GHC
import RtClosureInspect

import qualified Data.Map as M
import Control.Concurrent
import HscTypes


-- |
--
type EvalString = String


-- |
--
type MVarDAPContext = MVar DAPContext


-- |
--
data DAPContext = DAPContext {
    variableReferenceMapDAPContext :: M.Map Int (Term, EvalString)
  , bindingDAPContext :: [GHC.TyThing]
  , frameIdDAPContext :: Int
  , srcBPsDAPContext  :: M.Map Int FilePath
  , funcBPsDAPContext :: [Int]
  , traceCmdExecResultDAPContext   :: [Maybe GHC.ExecResult]
  , doContinueExecResultDAPContext :: [GHC.ExecResult]
  , runStmtDeclExceptionDAPContext :: [SourceError]
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
  , traceCmdExecResultDAPContext = []
  , doContinueExecResultDAPContext = []
  , runStmtDeclExceptionDAPContext = []
  }

  