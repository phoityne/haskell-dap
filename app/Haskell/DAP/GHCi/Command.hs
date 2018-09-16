{-# LANGUAGE LambdaCase #-}

module Haskell.DAP.GHCi.Command where

import qualified GHC
import GhcMonad
import HscTypes
import RdrName
import Outputable
import PprTyThing
import Debugger
import Exception
import FastString
import DataCon
import DynFlags
import RtClosureInspect
import qualified GHCi.UI as GHCi
import qualified GHCi.UI.Monad as GHCi hiding (runStmt)

import Control.DeepSeq (deepseq)
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
dapCommands :: MVar DAPContext -> [GHCi.Command]
dapCommands ctx = map mkCmd [
    ("dap-echo",            dapEcho,                                   noCompletion)
  , ("dap-scopes",          dapCmdRunner dapScopesCommand ctx,         noCompletion)
  , ("dap-set-breakpoints", dapCmdRunner dapSetBreakpointsCommand ctx, noCompletion)
  , ("dap-set-function-breakpoints"
                  , dapCmdRunner dapSetFunctionBreakpointsCommand ctx, noCompletion)
  , ("dap-continue",        dapCmdRunner dapContinueCommand ctx,       noCompletion)
  , ("dap-next",            dapCmdRunner dapNextCommand ctx,           noCompletion)
  , ("dap-step-in",         dapCmdRunner dapStepInCommand ctx,         noCompletion)
  , ("dap-stacktrace",      dapCmdRunner dapStackTraceCommand ctx,     noCompletion)
  , ("dap-variables",       dapCmdRunner dapVariablesCommand ctx,      noCompletion)
  , ("dap-evaluate",        dapCmdRunner dapEvaluateCommand ctx,       noCompletion)
  ]
  where
    mkCmd (n,a,c) = GHCi.Command { GHCi.cmdName = n
                              , GHCi.cmdAction = a
                              , GHCi.cmdHidden = False
                              , GHCi.cmdCompletionFunc = c
                              }


-- |
--
dapCmdRunner :: (MVar DAPContext -> String -> GHCi.GHCi ())
             ->  MVar DAPContext -> String -> InputT GHCi.GHCi Bool
dapCmdRunner cmd ctxMVar str = do
  
  lift $ cmd ctxMVar str

  liftIO $ putStrLn _DAP_CMD_END

  return False


------------------------------------------------------------------------------------------------
--  DAP Command :dap-echo
------------------------------------------------------------------------------------------------

-- |
--
dapEcho :: String -> InputT GHCi.GHCi Bool
dapEcho str = do
  liftIO $ putStrLn $ "[DAP][INFO] dap-echo \"" ++ str ++ "\""
  return False


------------------------------------------------------------------------------------------------
--  DAP Command :dap-scopes
------------------------------------------------------------------------------------------------

-- |
--
dapScopesCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapScopesCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res
  
  where
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let idx  = D.frameIdScopesArguments args
      getScopesBody idx

    -- |
    --
    getScopesBody :: Int -> GHCi.GHCi (Either String D.ScopesBody)
    getScopesBody curIdx = do
      -- liftIO $ putStrLn $ "[DAP][getScopesBody] frame id." ++ frameIdStr
      oldIdx <- liftIO $ frameIdDAPContext <$> readMVar ctxMVar
      let moveIdx = curIdx - oldIdx

      tyThings <- withMoveIdx moveIdx
      gobalTT  <- getGlobalBindings

      -- liftIO $ putStrLn $ "[DAP][getScopesBody] tyThings count." ++ show (length tyThings)
      ctx <- liftIO $ takeMVar ctxMVar
      liftIO $ putMVar ctxMVar ctx {
          variableReferenceMapDAPContext = M.empty
        , bindingDAPContext = tyThings
        , bindingGlobalDAPContext = gobalTT
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
          ,
          D.defaultScope{
              D.nameScope = _GHCi_GLOBAL_SCOPE
            , D.variablesReferenceScope = 2
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
    getGlobalBindings :: GhcMonad m => m [TyThing]
    getGlobalBindings = withSession $ \hsc_env -> do
      let ic = hsc_IC hsc_env
          gb = ic_rn_gbl_env ic
          es = globalRdrEnvElts gb
          ns = map gre_name es
      foldM withName [] $ reverse ns

    -- |
    --
    back num = do
      (names, _, _, _) <- GHC.back num
      st <- GHCi.getGHCiState
      enqueueCommands [GHCi.stop st]

      foldM withName [] $ reverse names

      
    -- |
    --
    forward num = do
      (names, _, _, _) <- GHC.forward num
      st <- GHCi.getGHCiState
      enqueueCommands [GHCi.stop st]

      foldM withName [] $ reverse names
           
    -- |
    --
    enqueueCommands :: [String] -> GHCi.GHCi ()
    enqueueCommands cmds = do
      -- make sure we force any exceptions in the commands while we're
      -- still inside the exception handler, otherwise bad things will
      -- happen (see #10501)
      cmds `deepseq` return ()
      GHCi.modifyGHCiState $ \st -> st{ GHCi.cmdqueue = cmds ++ GHCi.cmdqueue st }

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
dapSetBreakpointsCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapSetBreakpointsCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res

  where
    -- |
    --
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = getModule args >>= \case
      Left msg  -> return $ Left msg
      Right mod -> do      
        deleteBreakpoints mod
      
        addBreakpoints args mod


    -- |
    --
    getModule args = do
      let srcInfo = D.sourceSetBreakpointsArguments args
          srcPath = D.pathSource srcInfo

      modSums <- GHCi.getLoadedModules
      let modPaths = map takeModPath modSums

      case filter (isPathMatch srcPath) modPaths of
        ((m, p):[]) -> do
          liftIO $ putStrLn $ "[DAP][INFO][dapSetBreakpointsCommand] " ++ p ++ " -> " ++ m
          return $ Right m
        _ -> return $ Left $ "[DAP][ERROR] loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths


    -- |
    --
    deleteBreakpoints :: ModuleName -> GHCi.GHCi ()
    deleteBreakpoints mod = do
      bps <- liftIO $ getDelBPs mod

      liftIO $ putStrLn $ "[DAP][INFO][dapSetBreakpointsCommand] delete src bps " ++ show bps

      mapM_ delBreakpoint bps

    -- |
    --
    getDelBPs :: ModuleName -> IO [Int]
    getDelBPs mod = do
      ctx <- takeMVar ctxMVar

      let bpNOs = M.keys $ M.filter ((isModuleMatch mod)) $ srcBPsDAPContext ctx
          newSrcBPs = M.filter (not . (isModuleMatch mod)) $ srcBPsDAPContext ctx

      putMVar ctxMVar $ ctx {srcBPsDAPContext = newSrcBPs} 

      return bpNOs

    -- |
    --
    isModuleMatch :: ModuleName -> SourceBreakpointInfo -> Bool
    isModuleMatch mod bpInfo = mod == modNameSourceBreakpointInfo bpInfo


    -- |
    --
    takeModPath ms = (GHC.moduleNameString (GHC.ms_mod_name ms), GHC.ms_hspp_file ms)

    -- |
    --
    isPathMatch srcPath (_, p) = (nzPath srcPath) == (nzPath p)

    -- |
    --
    addBreakpoints :: D.SetBreakpointsArguments -> ModuleName -> GHCi.GHCi (Either String D.SetBreakpointsResponseBody)
    addBreakpoints args mod = do
      let srcBPs = D.breakpointsSetBreakpointsArguments args

      addBps <- mapM (addBP mod) srcBPs

      liftIO $ updateBpCtx addBps

      return $ Right $ D.SetBreakpointsResponseBody $ map takeBp addBps
    
    -- |
    --
    addBP :: String -> D.SourceBreakpoint -> GHCi.GHCi (ModuleName, D.SourceBreakpoint, D.Breakpoint)
    addBP mod srcBP = do
      let lineNo   = show $ D.lineSourceBreakpoint srcBP
          colNo    = getColNo $ D.columnSourceBreakpoint srcBP
          argStr   = mod ++ " " ++ lineNo ++ " " ++ colNo

      bp <- addBreakpoint argStr

      return (mod, srcBP, bp)

    -- |
    --
    getColNo :: Maybe Int -> String
    getColNo Nothing = ""
    getColNo (Just 1) = ""
    getColNo (Just a) = show a

    -- |
    --
    updateBpCtx :: [(ModuleName, D.SourceBreakpoint, D.Breakpoint)] -> IO ()
    updateBpCtx bps = do
      ctx <- takeMVar ctxMVar
      let cur = srcBPsDAPContext ctx
          new = M.fromList $ foldr convSrcBps [] bps
      putMVar ctxMVar $ ctx{srcBPsDAPContext = (M.union cur new)}

    -- |
    --
    convSrcBps :: (ModuleName, D.SourceBreakpoint, D.Breakpoint)
               -> [(Int, SourceBreakpointInfo)]
               -> [(Int, SourceBreakpointInfo)]
    convSrcBps (mod, srcBp, bp) acc = case D.idBreakpoint bp of
      Nothing -> acc
      Just no -> (no, SourceBreakpointInfo mod srcBp 0) : acc 

    -- |
    --
    takeBp :: (ModuleName, D.SourceBreakpoint, D.Breakpoint) -> D.Breakpoint
    takeBp (_, _, bp) = bp


------------------------------------------------------------------------------------------------
--  DAP Command :dap-set-function-breakpoints
------------------------------------------------------------------------------------------------

-- |
--
dapSetFunctionBreakpointsCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapSetFunctionBreakpointsCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res

  where
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) =  deleteBreakpoints
                          >> addBreakpoints args
      
    -- |
    --
    deleteBreakpoints :: GHCi.GHCi ()
    deleteBreakpoints = do
      bps <- liftIO $ getDelBPs

      liftIO $ putStrLn $ "[DAP][INFO][dapSetFunctionBreakpointsCommand] delete func bps " ++ show bps

      mapM_ delBreakpoint bps
      
    -- |
    --
    getDelBPs :: IO [Int]
    getDelBPs = do
      ctx <- takeMVar ctxMVar

      let bpNOs = M.keys $ funcBPsDAPContext ctx

      putMVar ctxMVar $ ctx {funcBPsDAPContext = M.fromList []} 

      return bpNOs

    -- |
    --
    addBreakpoints :: D.SetFunctionBreakpointsArguments -> GHCi.GHCi (Either String D.SetFunctionBreakpointsResponseBody)
    addBreakpoints args = do
      let funcBPs = D.breakpointsSetFunctionBreakpointsArguments args

      addBps <- mapM addBP funcBPs

      liftIO $ updateBpCtx addBps

      return $ Right $ D.SetFunctionBreakpointsResponseBody $ map snd addBps
    
    -- |
    --
    addBP :: D.FunctionBreakpoint -> GHCi.GHCi (D.FunctionBreakpoint, D.Breakpoint)
    addBP funcBP = do
      let argStr = D.nameFunctionBreakpoint funcBP

      bp <- addBreakpoint argStr

      return (funcBP, bp)

    -- |
    --
    updateBpCtx :: [(D.FunctionBreakpoint, D.Breakpoint)] -> IO ()
    updateBpCtx bps = do
      ctx <- takeMVar ctxMVar
      let new = foldr getBpNo [] bps

      putMVar ctxMVar $ ctx{funcBPsDAPContext = M.fromList new}

    -- |
    --
    getBpNo :: (D.FunctionBreakpoint, D.Breakpoint) -> [(Int, (D.FunctionBreakpoint, Int))] -> [(Int, (D.FunctionBreakpoint, Int))]
    getBpNo (funcBP, bp) acc = case D.idBreakpoint bp of
      Nothing -> acc
      Just no -> (no, (funcBP, 0)) : acc 


------------------------------------------------------------------------------------------------

-- |
--
delBreakpoint :: Int -> GHCi.GHCi Bool
delBreakpoint bpNoStr = do
  curSt <- GHCi.getGHCiState
  let curCount = GHCi.break_ctr curSt

  GHCi.deleteCmd (show bpNoStr)
  
  newSt <- GHCi.getGHCiState
  let newCount = GHCi.break_ctr newSt
  
  return (newCount == curCount - 1)


-- |
--
addBreakpoint :: String -> GHCi.GHCi D.Breakpoint
addBreakpoint argStr = do
  curSt <- GHCi.getGHCiState
  let curCount = GHCi.break_ctr curSt

  GHCi.breakCmd argStr
  
  newSt <- GHCi.getGHCiState
  let newCount = GHCi.break_ctr newSt
      isAdded = (newCount == curCount + 1)
      locMay  =  if isAdded then Just (head (GHCi.breaks newSt)) else Nothing
  
  withBreakLoc locMay

  where
    withBreakLoc (Just (no, bpLoc))= withSrcSpan no bpLoc (GHCi.breakLoc bpLoc)
    withBreakLoc Nothing = return D.defaultBreakpoint {
        D.verifiedBreakpoint = False
      , D.messageBreakpoint  = "[DAP][ERROR]set breakpoint seems to be failed."
      }

    withSrcSpan no bpLoc (GHC.RealSrcSpan dat) = return
      D.defaultBreakpoint {
        D.idBreakpoint        = Just no
      , D.verifiedBreakpoint  = True
      , D.sourceBreakpoint    = D.defaultSource {
          D.nameSource             = (Just . GHC.moduleNameString . GHC.moduleName . GHCi.breakModule) bpLoc
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
dapContinueCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapContinueCommand mvarCtx argsStr =   withArgs (readDAP argsStr) 
                                   >>= withStopResult

  where
  
    -- |
    --
    withArgs :: Either String D.ContinueArguments -> GHCi.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = case  D.exprContinueArguments args of
      Just expr -> runWithStmtTrace expr
      Nothing   -> runNoStmtTrace


    -- |
    --
    runWithStmtTrace expr = do
      clearTmpDAPContext

      GHCi.traceCmd expr

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

      GHCi.traceCmd ""

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
    withStopResult :: Either String D.StoppedEventBody -> GHCi.GHCi ()
    withStopResult res@(Right D.StoppedEventBody{D.reasonStoppedEventBody = "breakpoint"}) = breakthrough res
    withStopResult res = printDAP res


    -- |
    --
    breakthrough :: Either String D.StoppedEventBody -> GHCi.GHCi ()
    breakthrough res = isBreakthrough >>= \case
      False -> printDAP res
      True  -> runNoStmtTrace >>= withStopResult


    -- |
    --
    isBreakthrough :: GHCi.GHCi Bool
    isBreakthrough = GHC.getResumeContext >>= withResumes

    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withResumes :: [GHC.Resume] -> GHCi.GHCi Bool
    withResumes [] = do
      liftIO $ putStrLn "[DAP][WARN] invalid resume state."
      return False

    withResumes (r:_) =   pure (GHC.resumeBreakInfo r)
                      >>= GHCi.toBreakIdAndLocation
                      >>= withBreakInfo

    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withBreakInfo ::  Maybe (Int, GHCi.BreakLocation) -> GHCi.GHCi Bool
    withBreakInfo Nothing = do
      liftIO $ putStrLn "[DAP][WARN] invalid resume break info state."
      return False

    withBreakInfo (Just (no, _)) = findSrcBP no >>= \case
      Just srcBP -> withSrcBP no srcBP
      Nothing    -> findFuncBP no >>= \case
        Just fncBP -> withFuncBP no fncBP
        Nothing    -> do
          liftIO $ putStrLn $ "[DAP][WARN] invalid break no. " ++ show no
          return False

    -- |
    --
    findSrcBP no = do
      srcBPs <- liftIO $ srcBPsDAPContext <$> readMVar mvarCtx
      return $ M.lookup no srcBPs

    -- |
    --
    findFuncBP no = do
      funcBPs <- liftIO $ funcBPsDAPContext <$> readMVar mvarCtx
      return $ M.lookup no funcBPs

    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withSrcBP :: Int -> SourceBreakpointInfo -> GHCi.GHCi Bool
    withSrcBP no bpInfo = 
      let bpCond = D.conditionSourceBreakpoint (srcBPSourceBreakpointInfo bpInfo)
          bpLog  = D.logMessageSourceBreakpoint (srcBPSourceBreakpointInfo bpInfo)
      in
        srcBreakthroughCounterHandler no bpInfo >>= \case
          Just res -> return res
          Nothing -> breakthroughCondtionHandler no bpCond >>= \case
            Just res -> return res
            Nothing -> logPointHandler no bpLog >>= \case
              Just res -> return res
              Nothing  -> return False


    -- |
    --   @return
    --    True  -> thruough
    --    False -> break
    --
    withFuncBP no bpInfo = 
      let bpCond = D.conditionFunctionBreakpoint (fst bpInfo)
      in
        funcBreakthroughCounterHandler no bpInfo >>= \case
          Just res -> return res
          Nothing -> breakthroughCondtionHandler no bpCond >>= \case
            Just res -> return res
            Nothing  -> return False

    -- |
    --
    srcBreakthroughCounterHandler :: Int -> SourceBreakpointInfo -> GHCi.GHCi (Maybe Bool)
    srcBreakthroughCounterHandler _ SourceBreakpointInfo {
                                      srcBPSourceBreakpointInfo = D.SourceBreakpoint {
                                        D.hitConditionSourceBreakpoint = Nothing
                                      }
                                    } = return Nothing
    srcBreakthroughCounterHandler no bpInfo@SourceBreakpointInfo {
                                              srcBPSourceBreakpointInfo = D.SourceBreakpoint {
                                                  D.hitConditionSourceBreakpoint = Just condStr
                                              }
                                            , hitCntSourceBreakpointInfo = curCnt} = do
      let newCnt = curCnt + 1
          stmt   = "let _CNT = " ++ show newCnt ++ " in " ++ condStr

      liftIO $ updateSrcBreakCounter no bpInfo{hitCntSourceBreakpointInfo = newCnt}

      runStmtDAP mvarCtx False stmt >>= \case
        Left  err -> do
          liftIO $ putStrLn $ "[DAP][ERROR] hit condition statement fail. " ++ stmt ++ " -> " ++ err
          return $ Just False
        Right res -> do
          liftIO $ putStrLn $ "[DAP][INFO] hit condition statement result. " ++ stmt ++ " -> " ++ show res
          return $ Just ("False" == D.resultEvaluateBody res)

    -- |
    --
    updateSrcBreakCounter no bpInfo = do
      ctx <- takeMVar mvarCtx
      let cur = srcBPsDAPContext ctx
          new = M.insert no bpInfo cur
      putMVar mvarCtx ctx{srcBPsDAPContext = new}


    -- |
    --
    funcBreakthroughCounterHandler :: Int -> (D.FunctionBreakpoint, Int) -> GHCi.GHCi (Maybe Bool)
    funcBreakthroughCounterHandler _ (D.FunctionBreakpoint{D.hitConditionFunctionBreakpoint = Nothing}, _) = return Nothing
    funcBreakthroughCounterHandler no info@(D.FunctionBreakpoint{D.hitConditionFunctionBreakpoint = Just condStr}, curCnt) = do
      let newCnt = curCnt + 1
          stmt   = "let _CNT = " ++ show newCnt ++ " in " ++ condStr

      liftIO $ updateFuncBreakCounter no (fst info, newCnt)

      runStmtDAP mvarCtx False stmt >>= \case
        Left  err -> do
          liftIO $ putStrLn $ "[DAP][ERROR] hit condition statement fail. " ++ stmt ++ " -> " ++ err
          return $ Just False
        Right res -> do
          when ("Bool" /= D.typeEvaluateBody res) $
            liftIO $ putStrLn $ "[DAP][INFO] hit condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show res
          return $ Just ("False" == D.resultEvaluateBody res)


    -- |
    --
    updateFuncBreakCounter no bpInfo = do
      ctx <- takeMVar mvarCtx
      let cur = funcBPsDAPContext ctx
          new = M.insert no bpInfo cur
      putMVar mvarCtx ctx{funcBPsDAPContext = new}

    -- |
    --   @return
    --     True  -> breakthrough
    --     False -> break
    --
    breakthroughCondtionHandler :: Int -> Maybe String -> GHCi.GHCi (Maybe Bool)
    breakthroughCondtionHandler _ Nothing = return Nothing
    breakthroughCondtionHandler no (Just stmt) = runStmtDAP mvarCtx False stmt >>= \case
      Left  err -> do
        liftIO $ putStrLn $ "[DAP][ERROR] condition statement fail. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ err
        return $ Just False
      Right res -> do
        when ("Bool" /= D.typeEvaluateBody res) $
          liftIO $ putStrLn $ "[DAP][ERROR] condition statement result type is not Bool. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ show res
        return $ Just ("False" == D.resultEvaluateBody res)

    -- |
    --   @return
    --     must be True -> breakthrough
    -- 
    logPointHandler :: Int -> Maybe String -> GHCi.GHCi (Maybe Bool)
    logPointHandler _ Nothing = return Nothing
    logPointHandler no (Just stmt) = runStmtDAP mvarCtx False stmt >>= \case
      Left err -> do
        let msg = "[DAP][ERROR] log statement fail. BPNO:" ++ show no ++ " " ++ stmt ++ " -> " ++ err
            body = D.defaultOutputEventBody { D.outputOutputEventBody = msg
                                            , D.categoryOutputEventBody = "stderr" }
        liftIO $ putStrLn msg

        printOutputEventDAP (Right body)
        
        return $ Just False

      Right res -> do
        let body = D.defaultOutputEventBody {D.outputOutputEventBody = D.resultEvaluateBody res}

        printOutputEventDAP (Right body)

        return $ Just True


-- |
--
withExecResult :: MVar DAPContext -> String-> GHC.ExecResult -> GHCi.GHCi (Either String D.StoppedEventBody)
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
dapNextCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapNextCommand mvarCtx argsStr = do
  res <- withArgs (readDAP argsStr)
  printDAP res

  where
    withArgs :: Either String D.NextArguments -> GHCi.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = do
      clearTmpDAPContext

      GHCi.stepLocalCmd ""

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
dapStepInCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapStepInCommand mvarCtx argsStr = do
  res <- withArgs (readDAP argsStr)
  printDAP res
  
  where
    withArgs :: Either String D.StepInArguments -> GHCi.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = do
      clearTmpDAPContext

      GHCi.stepCmd ""

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
dapStackTraceCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapStackTraceCommand ctxMVar argsStr = do
  
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {frameIdDAPContext = 0}

  res <- withArgs (readDAP argsStr) 
  printDAP res
  
  where
    withArgs :: Either String D.StackTraceArguments -> GHCi.GHCi (Either String D.StackTraceBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = GHC.getResumeContext >>= \case
      [] -> return $ Left "no stacktrace found."
      (r:_) -> withResume r

    withResume r = case isExceptionResume r of
      True -> do
        dflags <- GHCi.getDynFlags
        let maxSize = GHC.ghciHistSize dflags
        -- liftIO $ putStrLn $ "[DAP][INFO] " ++ show maxSize

        traces <- mapM resumeHist2stackFrame $ take maxSize $ GHC.resumeHistory r

        return $ Right D.defaultStackTraceBody {
            D.stackFramesStackTraceBody = traces
          , D.totalFramesStackTraceBody = length traces
          }
      False -> do
        dflags <- GHCi.getDynFlags
        let start  = resume2stackframe r
            maxSize = (GHC.ghciHistSize dflags) - 1
        -- liftIO $ putStrLn $ "[DAP][INFO] " ++ show maxSize

        hists <- mapM resumeHist2stackFrame $ take maxSize $ GHC.resumeHistory r
        
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
dapVariablesCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapVariablesCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res

  where
    withArgs :: Either String D.VariablesArguments -> GHCi.GHCi (Either String D.VariablesBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let idx  = D.variablesReferenceVariablesArguments args

      vals <- getBindingVariables ctxMVar idx

      return $ Right $ D.VariablesBody $  L.sortBy compName vals

    compName a b = compare (D.nameVariable a) (D.nameVariable b)

-- |
--
getBindingVariables :: MVar DAPContext -> Int -> GHCi.GHCi [D.Variable]
getBindingVariables ctx idx
  | 1 == idx = getBindingVariablesLocal ctx 
  | 2 == idx = getBindingVariablesGlobal ctx 
  | otherwise  = getBindingVariablesNode ctx idx


-- |
--
getBindingVariablesLocal :: MVar DAPContext -> GHCi.GHCi [D.Variable]
getBindingVariablesLocal ctxMVar = do
  bindings <- liftIO $ bindingDAPContext <$> readMVar ctxMVar
  getBindingVariablesRoot  ctxMVar bindings


-- |
--
getBindingVariablesGlobal :: MVar DAPContext -> GHCi.GHCi [D.Variable]
getBindingVariablesGlobal ctxMVar = do
  bindings <- liftIO $ bindingGlobalDAPContext <$> readMVar ctxMVar
  getBindingVariablesRoot  ctxMVar bindings
  

-- |
--
getBindingVariablesRoot :: MVar DAPContext -> [GHC.TyThing] -> GHCi.GHCi [D.Variable]
getBindingVariablesRoot ctxMVar bindings = do
  -- bindings <- liftIO $ bindingDAPContext <$> readMVar ctxMVar
  -- liftIO $ putStrLn $ "[DAP][INFO] bindings " ++ show (length bindings)

  foldM go [] bindings
  --mapM tyThing2Val bindings

  where
    go acc ty = gcatch (doSomething acc ty) (onError acc)
    doSomething acc ty = do
      v <- tyThing2Val ty
      return (v:acc)
    onError :: [D.Variable] -> SomeException -> GHCi.GHCi [D.Variable]
    onError acc e = do
      liftIO $ putStrLn $ "[DAP][DEBUG] ERROR: " ++ (show e)
      return acc
      
    -- |
    --  TyThings https://hackage.haskell.org/package/ghc-8.2.1/docs/HscTypes.html#t:TyThing
    --
    tyThing2Val :: GHC.TyThing -> GHCi.GHCi D.Variable
    tyThing2Val (AnId i) = do
      let isForce = True
          depth   = _BINDING_INSPECT_DEPTH
          
      GHC.obtainTermFromId depth isForce i >>= withTerm i
   
    tyThing2Val t@(ATyCon c) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr t)
      , D.typeVariable  = showSDoc dflags (ppr c)
      , D.valueVariable = "<define>"
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }
  
    tyThing2Val t@(AConLike c) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr t)
      , D.typeVariable  = showSDoc dflags (ppr c)
      , D.valueVariable = "<define>"
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }
    
    tyThing2Val x = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = showSDoc dflags (ppr x)
      , D.typeVariable  = "not yet supported tything."
      , D.valueVariable = "not yet supported tything."
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm ::  GHC.Id -> Term -> GHCi.GHCi D.Variable
    withTerm i t@(Term ty _ _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let nameStr = showSDoc dflags (ppr i)
          typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- getNextIdx ctxMVar t nameStr
      
      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = nextIdx
      }

    withTerm i _ = do
      dflags <- getDynFlags
      idSDoc   <- pprTypeAndContents i

      let (nameStr, typeStr, valStr) = getNameTypeValue (showSDoc dflags idSDoc)
      {-
      let nameStr = takeWhile (/= ' ') $ showSDoc dflags idSDoc
          typeStr = showSDoc dflags idSDoc
          valStr = "<define>"
      -}

      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Nothing
      , D.variablesReferenceVariable = 0
      }


-- |
--
getNextIdx :: MVar DAPContext -> Term -> String -> GHCi.GHCi Int
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
      nextId = (M.size curMap) + 10

  putMVar ctxMVar $ ctx {variableReferenceMapDAPContext = M.insert nextId (t, str) curMap}

  return nextId


-- |
--
getDataConstructor :: Term -> GHCi.GHCi String
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
getBindingVariablesNode :: MVar DAPContext -> Int -> GHCi.GHCi [D.Variable]
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
dapEvaluateCommand :: MVar DAPContext -> String -> GHCi.GHCi ()
dapEvaluateCommand ctxMVar argsStr = do
  res <- withArgs (readDAP argsStr) 
  printDAP res

  where
    -- |
    --
    withArgs :: Either String D.EvaluateArguments -> GHCi.GHCi (Either String D.EvaluateBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = case D.contextEvaluateArguments args of
      Nothing     -> runRepl args
      Just "repl" -> runRepl args
      _           -> runOther args

    -- |
    --
    runRepl ::  D.EvaluateArguments -> GHCi.GHCi (Either String D.EvaluateBody)
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
    runOther ::  D.EvaluateArguments -> GHCi.GHCi (Either String D.EvaluateBody)
    runOther args = do 
      let nameStr = D.expressionEvaluateArguments args
      names <- gcatch (GHC.parseName nameStr) parseNameErrorHandler
      names2EvalBody ctxMVar True nameStr names


-- |
--
runStmtDAP :: MVar DAPContext -> Bool -> String -> GHCi.GHCi (Either String D.EvaluateBody)
runStmtDAP ctxMVar isRefable stmt = do
  clearTmpDAPContext

  GHCi.runStmt stmt GHC.RunToCompletion >>= \case
    Nothing -> Left <$> getRunStmtSourceError
--    Just (GHC.ExecBreak _ Nothing) -> Left <$> getRunStmtSourceError
--    Just (GHC.ExecBreak names _)   -> names2EvalBody ctxMVar isRefable stmt names
    Just (GHC.ExecBreak _ _) -> return $ Left $ "[DAP][ERROR] unexpected break result. "
    Just (GHC.ExecComplete (Left msg) _) -> return $ Left $ "[DAP][ERROR] error runStmt result. " ++ show msg
    Just (GHC.ExecComplete (Right names) _) -> names2EvalBody ctxMVar isRefable stmt names
    

-- |
--
--
names2EvalBody :: MVar DAPContext -> Bool -> String -> [GHC.Name] -> GHCi.GHCi (Either String D.EvaluateBody)
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
          depth   = _EVALUATE_INSPECT_DEPTH
      body  <- GHC.obtainTermFromId depth isForce i >>= withTerm i
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
    withTerm :: GHC.Id -> Term -> GHCi.GHCi D.EvaluateBody
    withTerm _ t@(Term ty _ _ _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      nextIdx <- if True == isRefable then getNextIdx ctxMVar t key else return 0
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t

      -- liftIO $ putStrLn "[DAP][INFO] Term Not yet supported."

      return D.defaultEvaluateBody {
               D.resultEvaluateBody = delDQ typeStr valStr'
             , D.typeEvaluateBody   = typeStr
             , D.variablesReferenceEvaluateBody = nextIdx
             }

    withTerm _ t@(Prim ty _) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      -- liftIO $ putStrLn "[DAP][INFO] Prim Not yet supported."

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

    delDQ :: String -> String -> String
    delDQ typ val
      | (typ == "[Char]" || typ == "String")
        && length val > 2
        && head val == '"' && last val == '"' = tail $ init val 
      | otherwise = val

