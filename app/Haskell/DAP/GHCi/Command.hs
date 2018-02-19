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
import Data.List
import qualified GHCi.UI as G
import qualified GHCi.UI.Monad as G

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
  --, ("dap-force",           dapForceCommand ctx,          noCompletion)
  , ("dap-scopes",          dapScopesCommand ctx,         noCompletion)
  , ("dap-history",         dapHistoryCommand ctx,        noCompletion)
  , ("dap-set-breakpoints", dapSetBreakpointsCommand ctx, noCompletion)
  , ("dap-continue",        dapContinueCommand ctx,       noCompletion)
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
dapHistoryCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapHistoryCommand ctxMVar _ = do
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {frameIdDAPContext = 0}

  let outStr = _DAP_HEADER ++ " frame id cleared."
  
  liftIO $ putStrLn outStr

  return False


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
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let srcBPs  = D.sourceSetBreakpointsArguments args
          srcPath = D.pathSource srcBPs
      delSrcBreakpintsInFile ctxMVar srcPath

      modSums <- G.getLoadedModules
      let modPaths = map takeModPath modSums

      case filter (isPathMatch srcPath) modPaths of
        ((m, p):[]) -> do
          liftIO $ putStrLn $ "[DAP][INFO][dapBreakCommand] " ++ p ++ " -> " ++ m
          withMod args m
        _ -> return $ Left $ "[DAP][ERROR] loaded module can not find from path. <" ++ srcPath ++ "> " ++  show modPaths

    takeModPath ms = (GHC.moduleNameString (GHC.ms_mod_name ms), GHC.ms_hspp_file ms)

    isPathMatch srcPath (_, p) = (nzPath srcPath) == (nzPath p)

    withMod args mod = do
      let srcBPs = D.breakpointsSetBreakpointsArguments args
      addBps <- mapM (go mod) srcBPs

      liftIO $ mapM_ updateBreakpointTypeMap addBps

      return $ Right $ D.SetBreakpointsResponseBody addBps
    
    go mod srcBP = do
      curSt <- G.getGHCiState
      let curCount = G.break_ctr curSt
          lineNo   = show $ D.lineSourceBreakpoint srcBP
          colNo    = maybe "" show $ D.columnSourceBreakpoint srcBP
          argStr   = mod ++ " " ++ lineNo ++ " " ++ colNo

      lift $ G.breakCmd argStr
      
      newSt <- G.getGHCiState
      let newCount = G.break_ctr newSt
          isAdded = (newCount == curCount + 1)
          locMay  =  if isAdded then Just (head (G.breaks newSt)) else Nothing
      
      withBreakLoc locMay
      
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

    updateBreakpointTypeMap bp = case D.idBreakpoint bp of
      Nothing -> return ()
      Just no -> do
        -- D.verifiedBreakpoint should be False.
        ctx <- takeMVar ctxMVar
        let bpMap = bpTypeMapDAPContext ctx
            newMap = M.insert no SourceBreakpoint bpMap
        
        putMVar ctxMVar ctx {bpTypeMapDAPContext = newMap}
    

-- |
--
delSrcBreakpintsInFile :: MVar DAPContext -> String -> InputT G.GHCi Bool
delSrcBreakpintsInFile ctxMVar path = deleteBreakpintsInFile ctxMVar path SourceBreakpoint

-- |
--
delFuncBreakpintsInFile :: MVar DAPContext -> String -> InputT G.GHCi Bool
delFuncBreakpintsInFile ctxMVar path = deleteBreakpintsInFile ctxMVar path FunctionBreakpoint

-- |
--   delete all breakpoint in the file.
--
deleteBreakpintsInFile :: MVar DAPContext -> String -> BreakpointType -> InputT G.GHCi Bool
deleteBreakpintsInFile  ctxMVar path bpTye = do
  bpMap <- liftIO $ bpTypeMapDAPContext <$> readMVar ctxMVar
  st <- G.getGHCiState
  let fileBpNOs = map fst $ filter (byFile path) $ G.breaks st
      srcBpNOs  = M.keys $ M.filter ((==) bpTye) bpMap
      delBpNOs  = intersect fileBpNOs srcBpNOs

  mapM_ callDelete delBpNOs

  return False

  where
    byFile path (_, bs) = withSrcSpan path $ G.breakLoc bs
    
    withSrcSpan path (GHC.RealSrcSpan dat) = nzPath path == (nzPath . unpackFS . GHC.srcSpanFile) dat
    withSrcSpan _    (GHC.UnhelpfulSpan _) = False
    
    callDelete bpNo = do
      let bpNoStr = show bpNo
      liftIO $ putStrLn $ "[DAP][INFO] delete breakpoint " ++ bpNoStr
      lift $  G.deleteCmd bpNoStr


------------------------------------------------------------------------------------------------
--  DAP Command :dap-continue
------------------------------------------------------------------------------------------------

-- |
--
dapContinueCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapContinueCommand _ argsStr = do
  withArgs (readDAP argsStr) >>= \case
    res@(Left _) -> lift $ printDAP res
    _ -> return ()

  return False
  
  where
    withArgs :: Either String D.ContinueArguments ->  InputT G.GHCi (Either String D.StoppedEventBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = do
      let expr = maybe "" id $ D.exprContinueArguments args
      -- currently, thread id staticaly 0.
      -- DAP result is show in traceCmd. hacked.
      lift $ G.traceCmd expr

      return $ Right D.defaultStoppedEventBody


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
dapStackTraceCommand _ argsStr = do
  res <- withArgs (readDAP argsStr) 
  lift $ printDAP res
  return False
  
  where
    withArgs :: Either String D.StackTraceArguments ->  InputT G.GHCi (Either String D.StackTraceBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right _) = GHC.getResumeContext >>= \case
      [] -> return $ Left "no stacktrace found."
      (r:_) -> withResume r

    withResume r = do
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
      | 1 == length subTerms && isPrim (head subTerms) = return 0
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
showTermErrorHandler :: SomeException -> G.GHCi SDoc
showTermErrorHandler e = return $ text $ show e


-- |
--
getNameTypeValue :: String -> (String, String, String)
getNameTypeValue str = (strip nameStr, strip typeStr, strip valueStr)
  where
    nameStr   = takeWhile (/= ' ')  str
    typeStr   = takeWhile (/= '=')  $ drop 4 $ dropWhile (/= ' ') str 
    valueStr_ = tail $ dropWhile (/= '=') str
    valueStr  = if elem "->" (words typeStr) then "function :: " ++ typeStr
                  else valueStr_

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
    withArgs :: Either String D.EvaluateArguments -> InputT G.GHCi (Either String D.EvaluateBody)
    withArgs (Left err) = return $ Left $ "[DAP][ERROR] " ++  err ++ " : " ++ argsStr
    withArgs (Right args) = case D.contextEvaluateArguments args of
      "watch" -> do
        body <- lift $ getForceEvalBody ctxMVar $ D.expressionEvaluateArguments args
        return $ Right body
      xs -> return $ Left $ "not supported evaluate context [" ++ xs ++ "]."


-- |
--
parseNameErrorHandler :: SomeException -> G.GHCi [GHC.Name]
parseNameErrorHandler e = liftIO $ print e >> return []


-- |
--
--
getForceEvalBody :: MVar DAPContext -> String -> G.GHCi D.EvaluateBody
getForceEvalBody ctxMVar nameStr =
  gcatch (GHC.parseName nameStr) parseNameErrorHandler >>= withNames

  where
    withNames [] = return D.defaultEvaluateBody {
                            D.resultEvaluateBody = "Not in scope: " ++ nameStr
                          , D.typeEvaluateBody   = "force error."
                          , D.variablesReferenceEvaluateBody = 0
                          }
    withNames (n:[]) = GHC.lookupName n >>= \case
      Just ty -> withTyThing ty
      Nothing -> return D.defaultEvaluateBody {
                          D.resultEvaluateBody = "variable not found. " ++ nameStr
                        , D.typeEvaluateBody   = "force error."
                        , D.variablesReferenceEvaluateBody = 0
                        }
    withNames _ = return D.defaultEvaluateBody {
                           D.resultEvaluateBody = "ambiguous name" ++ nameStr
                         , D.typeEvaluateBody   = "force error."
                         , D.variablesReferenceEvaluateBody = 0
                         }

    withTyThing (AnId i) = do
      let isForce = True
      GHC.obtainTermFromId maxBound isForce i >>= withTerm i

    withTyThing x = do
      dflags <- getDynFlags
      return D.defaultEvaluateBody {
               D.resultEvaluateBody = "unsupported tything. " ++ showSDoc dflags (ppr x)
             , D.typeEvaluateBody   = "force error."
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

      nextIdx <- getNextIdx ctxMVar t nameStr
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t
      return D.defaultEvaluateBody {
               D.resultEvaluateBody = valStr'
             , D.typeEvaluateBody   = typeStr
             , D.variablesReferenceEvaluateBody = nextIdx
             }
    withTerm i _ = do
      dflags <- getDynFlags
      idSDoc <- pprTypeAndContents i
      let (_, typeStr, valStr) = getNameTypeValue (showSDoc dflags idSDoc)
      return D.defaultEvaluateBody {
               D.resultEvaluateBody = valStr
             , D.typeEvaluateBody  = typeStr
             , D.variablesReferenceEvaluateBody = 0
             }

