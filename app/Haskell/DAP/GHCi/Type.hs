module Haskell.DAP.GHCi.Type where

import qualified GHC
import RtClosureInspect

import qualified Data.Map as M
import Control.Concurrent
import HscTypes

import qualified GHCi.DAP.IFData as D


-- |
--
type EvalString = String


-- |
--
type ModuleName = String


-- |
--
type MVarDAPContext = MVar DAPContext

-- |
--
data SourceBreakpointInfo = SourceBreakpointInfo {
    modNameSourceBreakpointInfo :: ModuleName
  , srcBPSourceBreakpointInfo   :: D.SourceBreakpoint
  , hitCntSourceBreakpointInfo  :: Int
  } deriving (Show, Read, Eq)


-- |
--
data DAPContext = DAPContext {
    variableReferenceMapDAPContext :: M.Map Int (Term, EvalString)
  , bindingDAPContext :: [GHC.TyThing]
  , bindingGlobalDAPContext :: [GHC.TyThing]
  , frameIdDAPContext :: Int
  , srcBPsDAPContext  :: M.Map Int SourceBreakpointInfo
  , funcBPsDAPContext :: M.Map Int (D.FunctionBreakpoint, Int)
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
  , bindingGlobalDAPContext = []
  , frameIdDAPContext = 0
  , srcBPsDAPContext  = M.fromList []
  , funcBPsDAPContext = M.fromList []
  , traceCmdExecResultDAPContext = []
  , doContinueExecResultDAPContext = []
  , runStmtDeclExceptionDAPContext = []
  }

  