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
import qualified GHCi.UI.Monad as G
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Monad

import System.Console.Haskeline
import RtClosureInspect
import qualified Data.Map as M
import qualified GHCi.DAP.Data as D


-- |
--
data DAPContext = DAPContext {
    variableReferenceMapDAPContext :: M.Map String (Term, String) 
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
--  let outStr = _DAP_HEADER ++ "{\"seq\":9,\"type\":\"response\",\"request_seq\":12,\"success\":true,\"command\":\"variables\",\"message\":\"\",\"body\":{\"variables\":[{\"name\":\"foo\",\"type\":\"Foo\",\"value\":\"Hoge aaa 2017\",\"evaluateName\":\"foo\",\"variablesReference\":0},{\"name\":\"_result\",\"type\":\"IO ()\",\"value\":\"_\",\"evaluateName\":\"_result\",\"variablesReference\":0}]}}"
--  let outStr = _DAP_HEADER ++ "[Variable {nameVariable = \"foo\", typeVariable = \"Foo\", valueVariable = \"Hoge aaa 2017\", evaluateNameVariable = Just \"foo\", variablesReferenceVariable = 0},Variable {nameVariable = \"_result\", typeVariable = \"IO ()\", valueVariable = \"_\", evaluateNameVariable = Just \"_result\", variablesReferenceVariable = 0}]"
--
dapBindingsCommand :: MVar DAPContext -> String -> InputT G.GHCi Bool
dapBindingsCommand ctx idxStr = do

  let isForce = False

  vals <- lift $ getBindigVariables ctx isForce idxStr

  let outStr = _DAP_HEADER ++ (show vals)
  
  liftIO $ putStrLn outStr

  return False


-- |
--
getBindigVariables :: MVar DAPContext -> Bool -> String -> G.GHCi [D.Variable]
getBindigVariables ctx isForce idStr 
  | "1" == idStr = getBindigVariablesRoot ctx isForce
  | otherwise    = getBindigVariablesNode ctx isForce idStr 



-- |
--
getBindigVariablesRoot :: MVar DAPContext -> Bool -> G.GHCi [D.Variable]
getBindigVariablesRoot ctxMVar isForce = do
  liftIO $ initVariableReferenceMapDAP ctxMVar
  bindings <- GHC.getBindings
  liftIO $ putStrLn $ "[INFO] bindings " ++ show (length bindings)
  rs <- GHC.getResumeContext
  when (length rs  > 0) $ do
    let tys = fst . GHC.resumeBindings . head $ rs
    liftIO $ putStrLn $ "[INFO] resume tys " ++ show (length tys)
  vals <- mapM tyThing2Val bindings

  return vals

  where
    -- |
    --  TyThings https://hackage.haskell.org/package/ghc-8.2.1/docs/HscTypes.html#t:TyThing
    --
    tyThing2Val ty@(AnId i) = do
      GHC.obtainTermFromId maxBound isForce i >>= withTerm i
    tyThing2Val x = do
      return D.getDefaultVariable {
        D.nameVariable  = showSDocUnsafe (ppr x)
      , D.typeVariable  = "not yet supported tything."
      , D.valueVariable = "not yet supported tything."
      , D.evaluateNameVariable = Just (showSDocUnsafe (ppr x))
      , D.variablesReferenceVariable = 0
      }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm ::  GHC.Id -> Term -> G.GHCi D.Variable
    withTerm i t@(Term ty _ _ subTerms) = do
      liftIO $ putStrLn $ "[DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let nameStr = showSDocUnsafe (ppr i)
          typeStr = showSDocUnsafe (pprTypeForUser ty)
          valStr  = showSDocUnsafe termSDoc

      nextIdx <- getNextIdx ctxMVar t nameStr
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t
      return D.getDefaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr'
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = nextIdx
      }
    withTerm i _ = do
      idSDoc   <- pprTypeAndContents i
      let (nameStr, typeStr, valStr) = getNameTypeValue (showSDocUnsafe idSDoc)
      return D.getDefaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr
      , D.evaluateNameVariable = Just nameStr
      , D.variablesReferenceVariable = 0
      }


-- |
--
getNextIdx :: MVar DAPContext -> Term -> String -> G.GHCi Int
getNextIdx ctxMVar t@(Term ty _ _ subTerms) str
  | 0 == length subTerms = return 0
  | 1 == length subTerms && isPrim (head subTerms) = return 0
  | "[Char]" == showSDocUnsafe (pprTypeForUser ty) = return 0
  | "String" == showSDocUnsafe (pprTypeForUser ty) = return 0
  | otherwise = liftIO $ addTerm2VariableReferenceMap ctxMVar t str


-- |
--
getDataConstructor :: Term -> G.GHCi String
getDataConstructor (Term _ (Left dc) _ _) = return dc
getDataConstructor (Term _ (Right dc) _ _) = do
  let conStr = if isTupleDataCon dc then "Tuple" else showSDocUnsafe $ ppr $ dataConName dc
      conStr' = if ":" == conStr then "List" else conStr
      typeStr = showSDocUnsafe (pprTypeForUser (dataConRepType dc))
  return $ conStr' ++ " :: " ++ typeStr


-- |
--
getDataConstructor (Term ty _ _ _) = return $ showSDocUnsafe (pprTypeForUser ty)
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
getBindigVariablesNode :: MVar DAPContext -> Bool ->  String -> G.GHCi [D.Variable]
getBindigVariablesNode ctxMVar isForce idStr = do
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
      liftIO $ putStrLn $ "[DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler

      let nameStr = label
          typeStr = showSDocUnsafe (pprTypeForUser ty)
          valStr  = showSDocUnsafe termSDoc

      nextIdx <- getNextIdx ctxMVar t evalStr
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t
      return D.getDefaultVariable {
        D.nameVariable  = nameStr
      , D.typeVariable  = typeStr
      , D.valueVariable = valStr'
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = nextIdx
      }
    withSubTerm evalStr (label, t@(Prim ty val)) = do
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      return D.getDefaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDocUnsafe (pprTypeForUser ty)
      , D.valueVariable = showSDocUnsafe (ppr val)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, (Suspension ct ty val bt)) = do
      return D.getDefaultVariable {
        D.nameVariable  = label
      , D.typeVariable  = showSDocUnsafe (pprTypeForUser ty)
      , D.valueVariable = "function :: " ++ showSDocUnsafe (pprTypeForUser ty)
      , D.evaluateNameVariable = Just evalStr
      , D.variablesReferenceVariable = 0
      }
    withSubTerm evalStr (label, _) = do
      return D.getDefaultVariable {
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
    withNames [] = return D.getDefaultEvaluateBody {
                            D.resultEvaluateBody = "Not in scope: " ++ nameStr
                          , D.typeEvaluateBody   = "force error."
                          , D.variablesReferenceEvaluateBody = 0
                          }
    withNames (n:[]) = GHC.lookupName n >>= \case
      Just ty -> withTyThing ty
      Nothing -> return D.getDefaultEvaluateBody {
                          D.resultEvaluateBody = "variable not found. " ++ nameStr
                        , D.typeEvaluateBody   = "force error."
                        , D.variablesReferenceEvaluateBody = 0
                        }
    withNames _ = return D.getDefaultEvaluateBody {
                           D.resultEvaluateBody = "ambiguous name" ++ nameStr
                         , D.typeEvaluateBody   = "force error."
                         , D.variablesReferenceEvaluateBody = 0
                         }

    withTyThing t@(AnId i) = do
      let isForce = True
      GHC.obtainTermFromId maxBound isForce i >>= withTerm i

    withTyThing x = return D.getDefaultEvaluateBody {
                            D.resultEvaluateBody = "unsupported tything. " ++ showSDocUnsafe (ppr x)
                          , D.typeEvaluateBody   = "force error."
                          , D.variablesReferenceEvaluateBody = 0
                          }

    -- |
    --  Term https://hackage.haskell.org/package/ghc-8.2.1/docs/RtClosureInspect.html
    --
    withTerm :: GHC.Id -> Term -> G.GHCi D.EvaluateBody
    withTerm _ t@(Term ty _ _ subTerms) = do
      -- liftIO $ putStrLn $ "[DEBUG]" ++ "   subTerms. [" ++ show (length subTerms) ++ "]"
      termSDoc <- gcatch (showTerm t) showTermErrorHandler
      let typeStr = showSDocUnsafe (pprTypeForUser ty)
          valStr  = showSDocUnsafe termSDoc

      nextIdx <- getNextIdx ctxMVar t nameStr
      valStr' <- if 0 == nextIdx then return valStr
                   else  getDataConstructor t
      return D.getDefaultEvaluateBody {
               D.resultEvaluateBody = valStr'
             , D.typeEvaluateBody   = typeStr
             , D.variablesReferenceEvaluateBody = nextIdx
             }
    withTerm i _ = do
      idSDoc <- pprTypeAndContents i
      let (_, typeStr, valStr) = getNameTypeValue (showSDocUnsafe idSDoc)
      return D.getDefaultEvaluateBody {
               D.resultEvaluateBody = valStr
             , D.typeEvaluateBody  = typeStr
             , D.variablesReferenceEvaluateBody = 0
             }

