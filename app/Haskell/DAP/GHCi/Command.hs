{-# LANGUAGE LambdaCase #-}

module Haskell.DAP.GHCi.Command where

import qualified GHC
import HscTypes
import Outputable
import PprTyThing
import Debugger
import Exception
import FastString
import DataCon
import DynFlags
import RtClosureInspect
import qualified GHCi.UI as G
import qualified GHCi.UI.Monad as G hiding (runStmt)

import Control.DeepSeq (deepseq)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Monad

import qualified Data.Map as M
import qualified Data.List as L

import System.Console.Haskeline

import qualified GHCi.DAP.IFData as D
import Haskell.DAP.GHCi.Type
import Haskell.DAP.GHCi.Constant
import Haskell.DAP.GHCi.Utility


-- |
--
dapCommands :: MVar DAPContext -> [G.Command]
dapCommands ctx = map mkCmd [
    ("dap-echo",            dapEcho,                      noCompletion)
  , ("dap-scopes",          dapScopesCommand ctx,         noCompletion)
  , ("dap-set-breakpoints", dapSetBreakpointsCommand ctx, noCompletion)
  , ("dap-set-function-breakpoints"
                  , dapSetFunctionBreakpointsCommand ctx, noCompletion)
  , ("dap-continue",        dapContinueCommand ctx,       noCompletion)
  , ("dap-next",            dapNextCommand ctx,           noCompletion)
  , ("dap-step-in",         dapStepInCommand ctx,         noCompletion)
  , ("dap-stacktrace",      dapStackTraceCommand ctx,     noCompletion)
  , ("dap-variables",       dapVariablesCommand ctx,      noCompletion)
  , ("dap-evaluate",        dapEvaluateCommand ctx,       noCompletion)
  ]
  where
    mkCmd (n,a,c) = G.Command { G.cmdName = n
                              , G.cmdAction = a
                              , G.cmdHidden = False
                              , G.cmdCompletionFunc = c
                              }


------------------------------------------------------------------------------------------------
--  DAP Command :dap-echo
------------------------------------------------------------------------------------------------

-- |
--
dapEcho :: String -> InputT G.GHCi Bool
dapEcho str = do
  liftIO $ putStrLn $ "[DAP][INFO] dap-echo \"" ++ str ++ "\""
  return False


------------------------------------------------------------------------------------------------
--  DAP Command :dap-history
------------------------------------------------------------------------------------------------

-- |
--
{-
dapHistoryCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapHistoryCommand ctxMVar _ = do
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {frameIdDAPContext = 0}

  let outStr = _DAP_HEADER ++ " frame id cleared."
  
  liftIO $ putStrLn outStr

  return False
-}

------------------------------------------------------------------------------------------------
--  DAP Command :dap-scopes
------------------------------------------------------------------------------------------------

-- |
--
dapScopesCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapScopesCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False

  where
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let idx  = D.frameIdScopesArguments args
      lift $ getScopesBody idx

    -- |
    --
    getScopesBody :: Int -> G.GHCi (Either String D.ScopesBody)
    getScopesBody curIdx = do
      -- liftIO $ putStrLn $ "[DAP][getScopesBody] frame id." ++ frameIdStr
      oldIdx <- liftIO $ frameIdDAPContext <$> readMVar ctxMVar
      let moveIdx = curIdx - oldIdx

      tyThings <- withMoveIdx moveIdx

      -- liftIO $ putStrLn $ "[DAP][getScopesBody] tyThings count." ++ show (length tyThings)
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {
        variableReferenceMapDAPContext = M.empty
        , bindingDAPContext = tyThings
        , frameIdDAPContext = curIdx
        }
    
      return $ Right D.ScopesBody {
        D.scopesScopesBody = [
          D.defaultScope{
              D.nameScope = _GHCi_SCOPE
            , D.variablesReferenceScope = 1
            , D.namedVariablesScope = Nothing
            , D.indexedVariablesScope = Nothing
            , D.expensiveScope = False
            }
          ]
        }

    -- |
    --
    withMoveIdx moveIdx
      | 0 == moveIdx = GHC.getBindings
      | 0 < moveIdx = back moveIdx
      | otherwise = forward moveIdx
  
    -- |
    --
    back num = do
      (names, _, _, _) <- GHC.back num
      st <- G.getGHCiState
      enqueueCommands [G.stop st]

      foldM withName [] $ reverse names

      
    -- |
    --
    forward num = do
      (names, _, _, _) <- GHC.forward num
      st <- G.getGHCiState
      enqueueCommands [G.stop st]

      foldM withName [] $ reverse names
           
    -- |
    --
    enqueueCommands :: [String] -> G.GHCi ()
    enqueueCommands cmds = do
      -- make sure we force any exceptions in the commands while we're
      -- still inside the exception handler, otherwise bad things will
      -- happen (see #10501)
      cmds `deepseq` return ()
      G.modifyGHCiState $ \st -> st{ G.cmdqueue = cmds ++ G.cmdqueue st }

    -- |
    --
    withName acc n = GHC.lookupName n >>= \case
      Just ty -> return (ty : acc)
      Nothing ->  do
        dflags <- getDynFlags
        liftIO $ putStrLn $ "[DAP][ERROR][getScopesBody] variable not found. " ++ showSDoc dflags (ppr n)
        return acc

      
------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-breakpoints
------------------------------------------------------------------------------------------------

-- |
--
dapSetBreakpointsCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapSetBreakpointsCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False

  where
    -- |
    --
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) =  deleteBreakpoints args
                          >> addBreakpoints args

    -- |
    --
    deleteBreakpoints args = do
      let srcInfo = D.sourceSetBreakpointsArguments args
          srcPath = D.pathSource srcInfo

      bps <- liftIO $ getDelBPs (nzPath srcPath)

      liftIO $ putStrLn $ "[DAP][INFO][dapSetBreakpointsCommand] delete src bps " ++ show bps

      mapM_ (lift.delBreakpoint.show) bps
      
    -- |
    --
    getDelBPs srcPath = do
      ctx <- takeMVar ctxMVar

      let bpNOs = M.keys $ M.filter ((==) srcPath) $ srcBPsDAPContext ctx
          newSrcBPs = M.filter ((/=) srcPath) $ srcBPsDAPContext ctx

      putMVar ctxMVar $ ctx {srcBPsDAPContext = newSrcBPs} 

      return bpNOs

    -- |
    --
    addBreakpoints args = do
      let srcInfo = D.sourceSetBreakpointsArguments args
          srcPath = D.pathSource srcInfo

      modSums <- G.getLoadedModules
      let modPaths = map takeModPath modSums

      case filter (isPathMatch srcPath) modPaths of
        ((m, p):[]) -> do
          liftIO $ putStrLn $ "[DAP][INFO][dapSetBreakpointsCommand] " ++ p ++ " -> " ++ m
          withMod args m
        _ -> return $ Left $ "[DAP][ERROR] loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths

    -- |
    --
    takeModPath ms = (GHC.moduleNameString (GHC.ms_mod_name ms), GHC.ms_hspp_file ms)

    -- |
    --
    isPathMatch srcPath (_, p) = (nzPath srcPath) == (nzPath p)

    -- |
    --
    withMod args mod = do
      let srcBPs = D.breakpointsSetBreakpointsArguments args

      addBps <- mapM (addBP mod) srcBPs

      liftIO $ updateBpCtx addBps

      return $ Right $ D.SetBreakpointsResponseBody addBps
    
    -- |
    --
    addBP mod srcBP = do
      let lineNo   = show $ D.lineSourceBreakpoint srcBP
          colNo    = maybe "" show $ D.columnSourceBreakpoint srcBP
          argStr   = mod ++ " " ++ lineNo ++ " " ++ colNo

      lift $ addBreakpoint argStr

    -- |
    --
    updateBpCtx bps = do
      ctx <- takeMVar ctxMVar
      let cur = srcBPsDAPContext ctx
          new = M.fromList $ foldr getBpNoAndPath [] bps
      putMVar ctxMVar $ ctx{srcBPsDAPContext = (M.union cur new)}

    -- |
    --
    getBpNoAndPath bp acc = case D.idBreakpoint bp of
      Nothing -> acc
      Just no -> (no, (nzPath . D.pathSource . D.sourceBreakpoint) (bp)) : acc 


------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-function-breakpoints
------------------------------------------------------------------------------------------------

-- |
--
dapSetFunctionBreakpointsCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapSetFunctionBreakpointsCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False

  where
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) =  deleteBreakpoints
                          >> addBreakpoints args
      
    -- |
    --
    deleteBreakpoints = do
      bps <- liftIO $ getDelBPs

      liftIO $ putStrLn $ "[DAP][INFO][dapSetFunctionBreakpointsCommand] delete func bps " ++ show bps

      mapM_ (lift.delBreakpoint.show) bps
      
    -- |
    --
    getDelBPs = do
      ctx <- takeMVar ctxMVar

      let bpNOs = funcBPsDAPContext ctx

      putMVar ctxMVar $ ctx {funcBPsDAPContext = []} 

      return bpNOs

    -- |
    --
    addBreakpoints args = do
      let funcBPs = D.breakpointsSetFunctionBreakpointsArguments args

      addBps <- mapM addBP funcBPs

      liftIO $ updateBpCtx addBps

      return $ Right $ D.SetFunctionBreakpointsResponseBody addBps
    
    -- |
    --
    addBP funcBP = do
      let argStr = D.nameFunctionBreakpoint funcBP

      lift $ addBreakpoint argStr

    -- |
    --
    updateBpCtx bps = do
      ctx <- takeMVar ctxMVar
      let cur = funcBPsDAPContext ctx
          new = foldr getBpNo [] bps
      putMVar ctxMVar $ ctx{funcBPsDAPContext = cur ++ new}

    -- |
    --
    getBpNo bp acc = case D.idBreakpoint bp of
      Nothing -> acc
      Just no -> no : acc 


------------------------------------------------------------------------------------------------

-- |
--
delBreakpoint :: String -> G.GHCi Bool
delBreakpoint bpNoStr = do
  curSt <- G.getGHCiState
  let curCount = G.break_ctr curSt

  G.deleteCmd bpNoStr
  
  newSt <- G.getGHCiState
  let newCount = G.break_ctr newSt
  
  return (newCount == curCount - 1)


-- |
--
addBreakpoint :: String -> G.GHCi D.Breakpoint
addBreakpoint argStr = do
  curSt <- G.getGHCiState
  let curCount = G.break_ctr curSt

  G.breakCmd argStr
  
  newSt <- G.getGHCiState
  let newCount = G.break_ctr newSt
      isAdded = (newCount == curCount + 1)
      locMay  =  if isAdded then Just (head (G.breaks newSt)) else Nothing
  
  withBreakLoc locMay

  where
    withBreakLoc (Just (no, bpLoc))= withSrcSpan no bpLoc (G.breakLoc bpLoc)
    withBreakLoc Nothing = return D.defaultBreakpoint {
        D.verifiedBreakpoint = False
      , D.messageBreakpoint  = "[DAP][ERROR]set breakpoint seems to be failed."
      }

    withSrcSpan no bpLoc (GHC.RealSrcSpan dat) = return
      D.defaultBreakpoint {
        D.idBreakpoint        = Just no
      , D.verifiedBreakpoint  = True
      , D.sourceBreakpoint    = D.defaultSource {
          D.nameSource             = (Just . GHC.moduleNameString . GHC.moduleName . G.breakModule) bpLoc
        , D.pathSource             = (unpackFS . GHC.srcSpanFile) dat
        , D.sourceReferenceSource  = Nothing
        , D.origineSource          = Nothing
        }
      , D.lineBreakpoint      = GHC.srcSpanStartLine dat
      , D.columnBreakpoint    = GHC.srcSpanStartCol dat
      , D.endLineBreakpoint   = GHC.srcSpanEndLine dat
      , D.endColumnBreakpoint = GHC.srcSpanEndCol dat
      }

    withSrcSpan _ _ (GHC.UnhelpfulSpan _) = return D.defaultBreakpoint {
        D.verifiedBreakpoint = False
      , D.messageBreakpoint  = "[DAP][ERROR] UnhelpfulSpan breakpoint."
      }


------------------------------------------------------------------------------------------------
--  DAP Command :dap-continue
------------------------------------------------------------------------------------------------

-- |
--
dapContinueCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapContinueCommand mvarCtx argsStr = do
  res <- withArgs (readDAP argsStr)
  lift $ printDAP res

  return False
  
  where
    withArgs :: Either String D.ContinueArguments ->  InputT G.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = case  D.exprContinueArguments args of
      Just expr -> lift $ runWithStmtTrace expr
      Nothing   -> lift $ runNoStmtTrace

    -- |
    --
    runWithStmtTrace expr = do
      clearTmpDAPContext

      G.traceCmd expr

      ctx <- liftIO $ readMVar mvarCtx
      withStmtTraceResults $ traceCmdExecResultDAPContext ctx


    -- |
    --
    withStmtTraceResults [] = return $ Left $ "[DAP][ERROR] invalid trace arg result."
    withStmtTraceResults (res:[]) = withStmtTraceResult res
    withStmtTraceResults (res:_) = do
      liftIO $ putStrLn $ "[DAP][WARN] two or more trace arg results. use first result. "
      withStmtTraceResult res
    

    -- |
    --
    withStmtTraceResult (Just res) = withExecResult mvarCtx "breakpoint" res
    withStmtTraceResult Nothing = do
      -- runStmt expr error occurred.
      msg <- getRunStmtSourceError
      return $ Left $ msg


    -- |
    --
    runNoStmtTrace = do
      clearTmpDAPContext

      G.traceCmd ""

      ctx <- liftIO $ readMVar mvarCtx
      withNoStmtTraceResults $ doContinueExecResultDAPContext ctx


    -- |
    --
    withNoStmtTraceResults [] = return $ Left $ "[DAP][ERROR] invalid trace no arg result."
    withNoStmtTraceResults (res:[]) = withExecResult mvarCtx "breakpoint" res
    withNoStmtTraceResults (res:_) = do
      liftIO $ putStrLn $ "[DAP][WARN] two or more trace no arg results. use first result. "
      withExecResult mvarCtx "breakpoint" res


-- |
--
withExecResult :: MVar DAPContext -> String-> GHC.ExecResult -> G.GHCi (Either String D.StoppedEventBody)
withExecResult _ _ (GHC.ExecComplete { GHC.execResult = Right _ }) = do
  return $  Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = "complete"
            }
  
withExecResult _ _ (GHC.ExecComplete { GHC.execResult = Left (SomeException e)}) = do
  return $  Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = "complete"
            , D.descriptionStoppedEventBody = show e
            , D.textStoppedEventBody = show e
            }

withExecResult _ reason (GHC.ExecBreak{GHC.breakInfo = Just _}) = do
  return $   Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = reason
            }

withExecResult mvarCtx _ (GHC.ExecBreak{GHC.breakInfo = Nothing}) = do
  let key = "_exception"
  --runStmtDAP mvarCtx False key >>= \case
  gcatch (GHC.parseName key) parseNameErrorHandler >>= names2EvalBody mvarCtx False key >>= \case
    Left  msg  -> return $ Left $ "[DAP][ERROR] invalid _exception result." ++ msg
    Right body -> return $ Right D.defaultStoppedEventBody {
        D.reasonStoppedEventBody = "exception"
      , D.descriptionStoppedEventBody = D.resultEvaluateBody body
      , D.textStoppedEventBody = D.resultEvaluateBody body
      }

{-do
  evalBody <- getEvalBody "_exception" True
  return $   Right D.defaultStoppedEventBody {
              D.reasonStoppedEventBody = "exception"
            , D.descriptionStoppedEventBody = D.resultEvaluateBody evalBody
            , D.textStoppedEventBody = D.resultEvaluateBody evalBody
            }
            -}


------------------------------------------------------------------------------------------------
--  DAP Command :dap-next
------------------------------------------------------------------------------------------------

-- |
--
dapNextCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapNextCommand mvarCtx argsStr = do
  res <- lift $ withArgs (readDAP argsStr)
  lift $ printDAP res

  return False

  where
    withArgs :: Either String D.NextArguments -> G.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = do
      clearTmpDAPContext

      G.stepLocalCmd ""

      ctx <- liftIO $ readMVar mvarCtx
      withResults $ doContinueExecResultDAPContext ctx


    -- |
    --
    withResults [] = return $ Left $ "[DAP][ERROR] invalid stepLocalCmd result."
    withResults (res:[]) = withExecResult mvarCtx "step" res
    withResults (res:_) = do
      liftIO $ putStrLn $ "[DAP][WARN] two or more stepLocalCmd results. use first result. "
      withExecResult mvarCtx "step" res


------------------------------------------------------------------------------------------------
--  DAP Command :dap-step-in
------------------------------------------------------------------------------------------------

-- |
--
dapStepInCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapStepInCommand mvarCtx argsStr = do
  res <- lift $ withArgs (readDAP argsStr)
  lift $ printDAP res

  return False
  
  where
    withArgs :: Either String D.StepInArguments -> G.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = do
      clearTmpDAPContext

      G.stepCmd ""

      ctx <- liftIO $ readMVar mvarCtx
      withResults $ doContinueExecResultDAPContext ctx


    -- |
    --
    withResults [] = return $ Left $ "[DAP][ERROR] invalid stepCmd result."
    withResults (res:[]) = withExecResult mvarCtx "step" res
    withResults (res:_) = do
      liftIO $ putStrLn $ "[DAP][WARN] two or more stepCmd results. use first result. "
      withExecResult mvarCtx "step" res


------------------------------------------------------------------------------------------------
--  DAP Command :dap-stacktrace
------------------------------------------------------------------------------------------------

-- |
-- 
_MAX_STACK_TRACE_SIZE :: Int
_MAX_STACK_TRACE_SIZE = 50


-- |
--
dapStackTraceCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapStackTraceCommand ctxMVar argsStr = do
  
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {frameIdDAPContext = 0}

  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False
  
  where
    withArgs :: Either String D.StackTraceArguments ->  InputT G.GHCi (Either String D.StackTraceBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = GHC.getResumeContext >>= \case
      [] -> return $ Left "no stacktrace found."
      (r:_) -> withResume r

    withResume r = case isExceptionResume r of
      True -> do
        traces <- mapM resumeHist2stackFrame $ GHC.resumeHistory r

        return $ Right D.defaultStackTraceBody {
            D.stackFramesStackTraceBody = traces
          , D.totalFramesStackTraceBody = length traces
          }
      False -> do
        let start  = resume2stackframe r
        hists <- mapM resumeHist2stackFrame $ GHC.resumeHistory r
        
        let traces = start : hists

        return $ Right D.defaultStackTraceBody {
            D.stackFramesStackTraceBody = traces
          , D.totalFramesStackTraceBody = length traces
          }

    resume2stackframe r = D.defaultStackFrame {
        D.idStackFrame = 0
      , D.nameStackFrame = (getStackFrameTitle r)
      , D.sourceStackFrame = D.defaultSource {
          D.pathSource = getSrcPath (GHC.resumeSpan r)
        }
      , D.lineStackFrame = getStartLinet (GHC.resumeSpan r)
      , D.columnStackFrame = getStartCol (GHC.resumeSpan r)
      , D.endLineStackFrame = getEndLinet (GHC.resumeSpan r)
      , D.endColumnStackFrame = getEndCol (GHC.resumeSpan r)
      }
      
    getStackFrameTitle r =  maybe "unknown" (GHC.moduleNameString  . GHC.moduleName . GHC.breakInfo_module) (GHC.resumeBreakInfo r)
                         ++ "."
                         ++ GHC.resumeDecl r

    getSrcPath (GHC.RealSrcSpan dat) = (unpackFS . GHC.srcSpanFile) dat
    getSrcPath (GHC.UnhelpfulSpan _) = "UnhelpfulSpan"

    getStartLinet (GHC.RealSrcSpan dat) = GHC.srcSpanStartLine dat
    getStartLinet (GHC.UnhelpfulSpan _) = 0

    getStartCol (GHC.RealSrcSpan dat) = GHC.srcSpanStartCol dat
    getStartCol (GHC.UnhelpfulSpan _) = 0

    getEndLinet (GHC.RealSrcSpan dat) = GHC.srcSpanEndLine dat
    getEndLinet (GHC.UnhelpfulSpan _) = 0

    getEndCol (GHC.RealSrcSpan dat) = GHC.srcSpanEndCol dat
    getEndCol (GHC.UnhelpfulSpan _) = 0

    resumeHist2stackFrame hist = do
      span <- GHC.getHistorySpan hist
      return D.defaultStackFrame {
        D.idStackFrame = 0
      , D.nameStackFrame = L.intercalate ":" (GHC.historyEnclosingDecls hist)
      , D.sourceStackFrame = D.defaultSource {
          D.pathSource = getSrcPath span
        }
      , D.lineStackFrame = getStartLinet span
      , D.columnStackFrame = getStartCol span
      , D.endLineStackFrame = getEndLinet span
      , D.endColumnStackFrame = getEndCol span
      }
    


------------------------------------------------------------------------------------------------
--  DAP Command :dap-variables
------------------------------------------------------------------------------------------------

-- |
--
dapVariablesCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapVariablesCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False

  where
    withArgs :: Either String D.VariablesArguments -> InputT G.GHCi (Either String D.VariablesBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let idx  = D.variablesReferenceVariablesArguments args

      vals <- lift $ getBindingVariables ctxMVar idx

      return $ Right $ D.VariablesBody vals


-- |
--
getBindingVariables :: MVar DAPContext -> Int -> G.GHCi [D.Variable]
getBindingVariables ctx idx
  | 1 == idx = getBindingVariablesRoot ctx 
  | otherwise  = getBindingVariablesNode ctx idx


-- |
--
getBindingVariablesRoot :: MVar DAPContext -> G.GHCi [D.Variable]
getBindingVariablesRoot ctxMVar = do
  bindings <- liftIO $ bindingDAPContext <$> readMVar ctxMVar
  -- liftIO $ putStrLn $ "[DAP][INFO] bindings " ++ show (length bindings)

  mapM tyThing2Val bindings

  where
    -- |
    --  TyThings https://hackage.haskell.org/package/ghc-8.2.1/docs/HscTypes.html#t:TyThing
    --
    tyThing2Val (AnId i) = do
      let isForce = False
      GHC.obtainTermFromId maxBound isForce i >>= withTerm i
    tyThing2Val x = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr x)
      , D.typeVariable  = "not yet supported tything."
      , D.valueVariable = "not yet supported tything."
      , D.evaluateNameVariable = Just (showSDoc dflags (ppr x))
      , D.variablesReferenceVariable = 0
      }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm ::  GHC.Id -> Term -> G.GHCi D.Variable
    withTerm i t@(Term ty _ _ _) = do
      dflags <- getDynFlags
      -- liftIO $ putStrLn $ "[DAP][DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let nameStr = showSDoc dflags (ppr i)
          typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = 0
      }
    withTerm i _ = do
      dflags <- getDynFlags
      idSDoc   <- pprTypeAndContents i
      let (nameStr, typeStr, valStr) = getNameTypeValue (showSDoc dflags idSDoc)
      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = 0
      }


-- |
--
getNextIdx :: MVar DAPContext -> Term -> String -> G.GHCi Int
getNextIdx ctxMVar t@(Term ty _ _ subTerms) str = getDynFlags >>= withDynFlags
  where
    withDynFlags dflags 
      | 0 == length subTerms = return 0
      | 1 == length subTerms && isPrim (head subTerms)  = return 0
      | "[Char]" == showSDoc dflags (pprTypeForUser ty) = return 0
      | "String" == showSDoc dflags (pprTypeForUser ty) = return 0
      | otherwise = liftIO $ addTerm2VariableReferenceMap ctxMVar t str

getNextIdx ctxMVar t str = liftIO $ addTerm2VariableReferenceMap ctxMVar t str


-- |
--
addTerm2VariableReferenceMap :: MVar DAPContext -> Term -> String -> IO Int
addTerm2VariableReferenceMap ctxMVar t str = do
  ctx <- takeMVar ctxMVar
  let curMap = variableReferenceMapDAPContext ctx
      nextId = (M.size curMap) + 2

  putMVar ctxMVar $ ctx {variableReferenceMapDAPContext = M.insert nextId (t, str) curMap}

  return nextId


-- |
--
getDataConstructor :: Term -> G.GHCi String
getDataConstructor (Term _ (Left dc) _ _) = return dc
getDataConstructor (Term _ (Right dc) _ _) = do
  dflags <- getDynFlags
  let conStr  = if isTupleDataCon dc then "Tuple" else showSDoc dflags $ ppr $ dataConName dc
      conStr' = if ":" == conStr then "List" else conStr
      typeStr = showSDoc dflags (pprTypeForUser (dataConRepType dc))
  return $ conStr' ++ " :: " ++ typeStr
getDataConstructor _ = return "[getDataConstructor] not supported type."



-- |
--
getBindingVariablesNode :: MVar DAPContext -> Int -> G.GHCi [D.Variable]
getBindingVariablesNode ctxMVar idx = do
  ctx <- liftIO $ readMVar ctxMVar
  case M.lookup idx (variableReferenceMapDAPContext ctx) of
    Just (t, str)  -> withTerm t str
    Nothing -> do
      liftIO $ putStrLn $ "[DAP][ERROR][getBindingVariablesNode] id not found. " ++ show idx
      return []

  where
    withTerm (Term _ (Right dc) _ subTerms) str = do
      let labels = if 0 == length (dataConFieldLabels dc)
                     then map (\i->"_" ++ show i) [1..(length subTerms)]
                     else map (unpackFS . flLabel) (dataConFieldLabels dc)
      mapM (withSubTerm str) $ zip labels subTerms

    withTerm (Term _ (Left _) _ subTerms) str = do
      let labels = map (\i->"_" ++ show i) [1..(length subTerms)]
      mapM (withSubTerm str) $ zip labels subTerms

    withTerm _ _ = do
      liftIO $ putStrLn $ "[DAP][ERROR][getBindingVariablesNode] invalid map term type. " ++ show idx
      return []

    withSubTerm evalStr (label, t@(Term ty _ _ _)) = do
      -- liftIO $ putStrLn $ "[DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      dflags <- getDynFlags

      let nameStr = label
          typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- getNextIdx ctxMVar t evalStr
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t
      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr'
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = nextIdx
      }
    withSubTerm evalStr (label, (Prim ty val)) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDoc dflags (pprTypeForUser ty)
      , D.valueVariable = showSDoc dflags (ppr val)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, (Suspension _ ty _ _)) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDoc dflags (pprTypeForUser ty)
      , D.valueVariable = "function :: " ++ showSDoc dflags (pprTypeForUser ty)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, _) = return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = "not supported subTerm."
      , D.valueVariable = "not supported subTerm."
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }

------------------------------------------------------------------------------------------------
--  DAP Command :dap-evaluate
------------------------------------------------------------------------------------------------

-- |
--
dapEvaluateCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapEvaluateCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False

  where
    -- |
    --
    withArgs :: Either String D.EvaluateArguments -> InputT G.GHCi (Either String D.EvaluateBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = case D.contextEvaluateArguments args of
      "repl" -> lift $ runRepl args
      _      -> lift $ runOther args

    -- |
    --
    runRepl ::  D.EvaluateArguments -> G.GHCi (Either String D.EvaluateBody)
    runRepl args
      | null (D.expressionEvaluateArguments args) = return $ Right D.defaultEvaluateBody {
          D.resultEvaluateBody = "no input."
        , D.typeEvaluateBody   = "no input."
        , D.variablesReferenceEvaluateBody = 0
        }
      | otherwise = do
        let stmt = D.expressionEvaluateArguments args
            isRefable = True

        runStmtDAP ctxMVar isRefable stmt


    -- |
    --
    runOther ::  D.EvaluateArguments -> G.GHCi (Either String D.EvaluateBody)
    runOther args = do 
      let nameStr = D.expressionEvaluateArguments args
      names <- gcatch (GHC.parseName nameStr) parseNameErrorHandler
      names2EvalBody ctxMVar True nameStr names


-- |
--
runStmtDAP :: MVar DAPContext -> Bool -> String -> G.GHCi (Either String D.EvaluateBody)
runStmtDAP ctxMVar isRefable stmt = do
  clearTmpDAPContext

  G.runStmt stmt GHC.RunToCompletion >>= \case
    Nothing -> Left <$> getRunStmtSourceError
    Just (GHC.ExecBreak _ _) -> return $ Left $ "[DAP][ERROR] unexpected result ExecBreak."
    Just (GHC.ExecComplete (Left msg) _) -> return $ Left $ "[DAP][ERROR] error runStmt result. " ++ show msg
    Just (GHC.ExecComplete (Right names) _) -> names2EvalBody ctxMVar isRefable stmt names
    

-- |
--
--
names2EvalBody :: MVar DAPContext -> Bool -> String -> [GHC.Name] -> G.GHCi (Either String D.EvaluateBody)
names2EvalBody ctxMVar isRefable key names
  | 0 == length names = return $ Left $ "Not in scope. " ++ key
  | 1 == length names = withName $ head names
  | otherwise = return $ Left $ "Ambiguous name. " ++ key

  where
    withName n = GHC.lookupName n >>= \case
      Nothing -> return $ Left $ "TyThing not found. " ++ key
      Just ty -> withTyThing ty

    withTyThing (AnId i) = do
      let isForce = True
      body <- GHC.obtainTermFromId maxBound isForce i >>= withTerm i
      return $ Right body

    withTyThing x = do
      liftIO $ putStrLn "[DAP][INFO]  withTyThing x Not yet supported."
      dflags <- getDynFlags
      return $ Right D.defaultEvaluateBody {
               D.resultEvaluateBody = showSDoc dflags (ppr x)
             , D.typeEvaluateBody   = showSDoc dflags (ppr x)
             , D.variablesReferenceEvaluateBody = 0
             }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm :: GHC.Id -> Term -> G.GHCi D.EvaluateBody
    withTerm _ t@(Term ty _ _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- if True == isRefable then getNextIdx ctxMVar t key else return 0
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t

      liftIO $ putStrLn "[DAP][INFO] Term Not yet supported."

      return D.defaultEvaluateBody {
               D.resultEvaluateBody = valStr'
             , D.typeEvaluateBody   = typeStr
             , D.variablesReferenceEvaluateBody = nextIdx
             }

    withTerm _ t@(Prim ty _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] Prim Not yet supported."

      return D.defaultEvaluateBody {
                D.resultEvaluateBody = valStr
              , D.typeEvaluateBody   = typeStr
              , D.variablesReferenceEvaluateBody = 0
              }

    withTerm _ t@(Suspension clsr ty _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = "closure(" ++ show clsr ++ ")" ++ " :: " ++ showSDoc dflags (pprTypeForUser ty) ++ " # " ++ showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] Suspension Not yet supported."
      return D.defaultEvaluateBody {
                D.resultEvaluateBody = typeStr
              , D.typeEvaluateBody   = typeStr
              , D.variablesReferenceEvaluateBody = 0
              }

    withTerm _ (NewtypeWrap ty _ wt) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm wt) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] NewtypeWrap Not yet supported."
      return D.defaultEvaluateBody {
                D.resultEvaluateBody = valStr
              , D.typeEvaluateBody   = typeStr
              , D.variablesReferenceEvaluateBody = 0
              }

    withTerm _ (RefWrap ty wt) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm wt) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      liftIO $ putStrLn "[DAP][INFO] RefWrap Not yet supported."
      return D.defaultEvaluateBody {
                D.resultEvaluateBody = valStr
              , D.typeEvaluateBody   = typeStr
              , D.variablesReferenceEvaluateBody = 0
              }


