{-# LANGUAGE LambdaCase          #-}

module Haskell.DAP.GHCi.Command where

import qualified GHC
import HscTypes
import Outputable
import PprTyThing
import Debugger
import Exception
import FastString
import DataCon
import TyCoRep
import DynFlags
import qualified GHCi.UI.Monad as G
import Control.DeepSeq (deepseq)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Monad
import Data.Char

import System.Console.Haskeline
import RtClosureInspect
import qualified Data.Map as M
import qualified GHCi.DAP.Data as D

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
defaultDAPContext = DAPContext {
    variableReferenceMapDAPContext = M.fromList []
  , bindingDAPContext = []
  , frameIdDAPContext = 0
  }

-- |
--
initVariableReferenceMapDAP :: MVar DAPContext -> IO ()
initVariableReferenceMapDAP ctxMVar = do
  ctx <- takeMVar ctxMVar
  putMVar ctxMVar $ ctx {variableReferenceMapDAPContext = M.empty}


-- |
--
addTerm2VariableReferenceMap :: MVar DAPContext -> Term -> String -> IO Int
addTerm2VariableReferenceMap ctxMVar t str = do
  ctx <- takeMVar ctxMVar
  let curMap = variableReferenceMapDAPContext ctx
      nextId = (M.size curMap) + 2

  putMVar ctxMVar $ ctx {variableReferenceMapDAPContext = M.insert (show nextId) (t, str) curMap}

  return nextId

-- |
--
dapCommands :: MVar DAPContext -> [G.Command]
dapCommands ctx = map mkCmd [
    ("dap-echo",     dapEcho,                noCompletion)
  , ("dap-bindings", dapBindingsCommand ctx, noCompletion)
  , ("dap-force",    dapForceCommand ctx,    noCompletion)
  , ("dap-scopes",   dapScopesCommand ctx,   noCompletion)
  , ("dap-history",  dapHistoryCommand ctx,  noCompletion)
  ]
  where
    mkCmd (n,a,c) = G.Command { G.cmdName = n
                              , G.cmdAction = a
                              , G.cmdHidden = False
                              , G.cmdCompletionFunc = c
                              }

-- |
--
dapEcho :: String -> InputT G.GHCi Bool
dapEcho str = do
  liftIO $ putStrLn $ "[INFO] dap-echo \"" ++ str ++ "\""
  return False

-- |
--
_DAP_HEADER :: String
_DAP_HEADER = "<<DAP>>"


-- |
--
dapHistoryCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapHistoryCommand ctxMVar _ = do
  ctx <- liftIO $ takeMVar ctxMVar
  liftIO $ putMVar ctxMVar ctx {frameIdDAPContext = 0}

  let outStr = _DAP_HEADER ++ " frame id creared."
  
  liftIO $ putStrLn outStr

  return False


-- |
--
--  let outStr = _DAP_HEADER ++ "{\"seq\":9,\"type\":\"response\",\"request_seq\":12,\"success\":true,\"command\":\"variables\",\"message\":\"\",\"body\":{\"variables\":[{\"name\":\"foo\",\"type\":\"Foo\",\"value\":\"Hoge aaa 2017\",\"evaluateName\":\"foo\",\"variablesReference\":0},{\"name\":\"_result\",\"type\":\"IO ()\",\"value\":\"_\",\"evaluateName\":\"_result\",\"variablesReference\":0}]}}"
--  let outStr = _DAP_HEADER ++ "[Variable {nameVariable = \"foo\", typeVariable = \"Foo\", valueVariable = \"Hoge aaa 2017\", evaluateNameVariable = Just \"foo\", variablesReferenceVariable = 0},Variable {nameVariable = \"_result\", typeVariable = \"IO ()\", valueVariable = \"_\", evaluateNameVariable = Just \"_result\", variablesReferenceVariable = 0}]"
--
dapBindingsCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapBindingsCommand ctxMVar idxStr = do

  vals <- lift $ getBindigVariables ctxMVar idxStr

  let outStr = _DAP_HEADER ++ (show vals)
  
  liftIO $ putStrLn outStr

  return False


-- |
--
getBindigVariables :: MVar DAPContext -> String -> G.GHCi [D.Variable]
getBindigVariables ctx idStr 
  | "1" == idStr = getBindigVariablesRoot ctx 
  | otherwise    = getBindigVariablesNode ctx idStr 



-- |
--
getBindigVariablesRoot :: MVar DAPContext -> G.GHCi [D.Variable]
getBindigVariablesRoot ctxMVar = do
  bindings <- liftIO $ bindingDAPContext <$> readMVar ctxMVar
  liftIO $ putStrLn $ "[INFO] bindings " ++ show (length bindings)

  vals <- mapM tyThing2Val bindings

  return vals

  where
    -- |
    --  TyThings https://hackage.haskell.org/package/ghc-8.2.1/docs/HscTypes.html#t:TyThing
    --
    tyThing2Val ty@(AnId i) = do
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
    withTerm i t@(Term ty _ _ subTerms) = do
      dflags <- getDynFlags
      liftIO $ putStrLn $ "[DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let nameStr = showSDoc dflags (ppr i)
          typeStr = showSDoc dflags (pprTypeForUser ty)
          valStr  = showSDoc dflags termSDoc

      -- nextIdx <- getNextIdx ctxMVar t nameStr
      -- valStr' <- if 0 == nextIdx then return valStr
      --             else  getDataConstructor t
      return D.defaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      -- , D.valueVariable = valStr'
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = 0
      -- , D.variablesReferenceVariable = nextIdx
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


-- |
--
getDataConstructor (Term ty _ _ _) = do
  dflags <- getDynFlags
  return $ showSDoc dflags (pprTypeForUser ty)
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
lstrip, rstrip, strip :: String -> String
lstrip = dropWhile (== ' ')
rstrip = reverse . lstrip . reverse
strip  = lstrip . rstrip

-- |
--
getBindigVariablesNode :: MVar DAPContext -> String -> G.GHCi [D.Variable]
getBindigVariablesNode ctxMVar idStr = do
  ctx <- liftIO $ readMVar ctxMVar
  case M.lookup idStr (variableReferenceMapDAPContext ctx) of
    Just (t, str)  -> withTerm t str
    Nothing -> do
      liftIO $ putStrLn $ "[CRITICAL] id not found. " ++ idStr
      return []

  where
    withTerm (Term ty (Right dc) val subTerms) str = do
      let labels = if 0 == length (dataConFieldLabels dc)
                     then map (\i->"_" ++ show i) [1..(length subTerms)]
                     else map (unpackFS . flLabel) (dataConFieldLabels dc)
      mapM (withSubTerm str) $ zip labels subTerms

    withTerm (Term ty (Left _) val subTerms) str = do
      let labels = map (\i->"_" ++ show i) [1..(length subTerms)]
      mapM (withSubTerm str) $ zip labels subTerms

    withTerm _ _ = do
      liftIO $ putStrLn $ "[CRITICAL] invalid map term type. " ++ idStr
      return []

    withSubTerm evalStr (label, t@(Term ty dc val subTerms)) = do
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
    withSubTerm evalStr (label, t@(Prim ty val)) = do
      dflags <- getDynFlags
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDoc dflags (pprTypeForUser ty)
      , D.valueVariable = showSDoc dflags (ppr val)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, (Suspension ct ty val bt)) = do
      dflags <- getDynFlags
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDoc dflags (pprTypeForUser ty)
      , D.valueVariable = "function :: " ++ showSDoc dflags (pprTypeForUser ty)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, _) = do
      return D.defaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = "not supported subTerm."
      , D.valueVariable = "not supported subTerm."
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }


-- |
--
--
dapForceCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapForceCommand ctx valStr = do

  body <- lift $ getForceEvalBody ctx valStr

  let outStr = _DAP_HEADER ++ (show body)
  
  liftIO $ putStrLn outStr

  return False


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

    withTyThing t@(AnId i) = do
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
    withTerm _ t@(Term ty _ _ subTerms) = do
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


-- |
--
dapScopesCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapScopesCommand ctx idxStr = do

  vals <- lift $ getScopesBody ctx idxStr

  let outStr = _DAP_HEADER ++ (show vals)
  
  liftIO $ putStrLn outStr

  return False


-- |
--
_GHCi_SCOPE :: String
_GHCi_SCOPE = "GHCi Scope"


-- |
--
getScopesBody :: MVar DAPContext -> String -> G.GHCi D.ScopesBody
getScopesBody ctxMVar frameIdStr 
  | all isDigit frameIdStr = do
    liftIO $ putStrLn $ "[getScopesBody] frame id." ++ frameIdStr
    oldIdx <- liftIO $ frameIdDAPContext <$> readMVar ctxMVar
    let curIdx  = read frameIdStr
        moveIdx = curIdx - oldIdx

    tyThings <- withMoveIdx moveIdx

    liftIO $ putStrLn $ "[getScopesBody] tyThings count." ++ show (length tyThings)
    ctx <- liftIO $ takeMVar ctxMVar
    liftIO $ putMVar ctxMVar ctx {
       variableReferenceMapDAPContext = M.empty
      , bindingDAPContext = tyThings
      , frameIdDAPContext = curIdx
      }
  
    return D.ScopesBody {
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
  | otherwise = do
    liftIO $ putStrLn $ "[getScopesBody] invalid frame id." ++ frameIdStr
    return D.ScopesBody {
      D.scopesScopesBody = [D.defaultScope{D.nameScope = "invalid frame id." ++ frameIdStr}]
    }

  where
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
        liftIO $ putStrLn $ "[DEBUG] variable not found. " ++ showSDoc dflags (ppr n)
        return acc
    
  