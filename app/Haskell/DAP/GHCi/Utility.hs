{-# LANGUAGE LambdaCase #-}

module Haskell.DAP.GHCi.Utility where

import qualified GHC
import qualified GHCi.UI.Monad as G
import HscTypes
import Outputable
import PprTyThing
import Debugger
import Exception
import DynFlags
import RtClosureInspect

import qualified Data.Char as CH
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Read as R
import Data.Word

import Control.Monad.IO.Class

import Haskell.DAP.GHCi.Constant
import qualified GHCi.DAP.IFData as D

-- |
--
_SLASH :: Char
_SLASH = '/'


-- |
--
_BACK_SLASH :: Char
_BACK_SLASH = '\\'


-- |
--
_SPACES :: [Char]
_SPACES = [' ', '\n', '\t']


-- |
--
lstrip, rstrip, strip :: String -> String
lstrip = dropWhile (flip elem _SPACES)
rstrip = reverse . lstrip . reverse
strip  = lstrip . rstrip


-- |
--
toLower :: String -> String
toLower = map CH.toLower


-- |
--
toUpper :: String -> String
toUpper = map CH.toUpper


-- |
--
win2unixSlash :: String -> String
win2unixSlash = map (\c -> if c == _BACK_SLASH then _SLASH else c)


-- |
--
unix2winSlash :: String -> String
unix2winSlash = map (\c -> if c == _SLASH then _BACK_SLASH else c)


-- |
--   normalized path
--
nzPath :: FilePath -> FilePath
nzPath = drive2lower . win2unixSlash


-- |
--  to lowercase Windows drive letter 
-- 
drive2lower :: FilePath -> FilePath
drive2lower (x : ':' : xs) = CH.toLower x : ':' : xs
drive2lower xs = xs


------------------------------------------------------------------------------------------------
--  DAP Utility
------------------------------------------------------------------------------------------------

-- |
--
readDAP :: Read a => String -> Either String a
readDAP argsStr = case R.readEither argsStr :: Either String [Word8] of
  Left err -> Left $ "read [Word8] failed. " ++ err ++ " : " ++ argsStr
  Right bs -> case R.readEither (toStr bs) of
    Left err -> Left $ "read response body failed. " ++ err ++ " : " ++  (toStr bs)
    Right a  -> Right a 
  where
    toStr = T.unpack . T.decodeUtf8 . BS.pack


-- |
--
showDAP :: Show a => a -> String
showDAP = show . BS.unpack . T.encodeUtf8 . T.pack . show


-- |
--
printDAP :: Show a => a -> G.GHCi ()
printDAP dat = do
  let outStr = _DAP_HEADER ++ showDAP dat

  liftIO $ putStrLn outStr


-- |
--
showStoppedEventBody :: Maybe GHC.ExecResult -> G.GHCi ()
showStoppedEventBody Nothing = return ()
showStoppedEventBody (Just (GHC.ExecComplete { GHC.execResult = Right _ })) = do
  let body = Right D.defaultStoppedEventBody {
             D.reasonStoppedEventBody = "complete"
           } :: Either String D.StoppedEventBody
      outStr = _DAP_HEADER ++ showDAP body
  liftIO $ putStrLn "[DAP][INFO] debug complete"
  liftIO $ putStrLn outStr
  
showStoppedEventBody (Just (GHC.ExecComplete { GHC.execResult = Left (SomeException e)})) = do
  let body = Right D.defaultStoppedEventBody {
             D.reasonStoppedEventBody = "complete"
           , D.descriptionStoppedEventBody = show e
           , D.textStoppedEventBody = show e
           } :: Either String D.StoppedEventBody
      outStr = _DAP_HEADER ++ showDAP body
  liftIO $ putStrLn $ "[DAP][INFO] debug complete with exception. " ++ show e
  liftIO $ putStrLn outStr

showStoppedEventBody (Just GHC.ExecBreak{}) = getExceptionResume >>= \case
  Nothing  -> stoppedByBreak
  Just _  -> stoopedWithException

  where
    stoopedWithException = do
      evalBody <- getEvalBody "_exception" True
      let body = Right D.defaultStoppedEventBody {
                 D.reasonStoppedEventBody = "exception"
               , D.descriptionStoppedEventBody = D.resultEvaluateBody evalBody
               , D.textStoppedEventBody = D.resultEvaluateBody evalBody
               } :: Either String D.StoppedEventBody
          outStr = _DAP_HEADER ++ showDAP body
      liftIO $ putStrLn "[DAP][INFO] break with exception."
      liftIO $ putStrLn outStr

    stoppedByBreak = do
      let body = Right D.defaultStoppedEventBody {
                 D.reasonStoppedEventBody = "step"
               } :: Either String D.StoppedEventBody
          outStr = _DAP_HEADER ++ showDAP body

      liftIO $ putStrLn "[DAP][INFO] breaks."
      liftIO $ putStrLn outStr


-- |
--
getExceptionResume :: G.GHCi (Maybe GHC.Resume)
getExceptionResume = GHC.getResumeContext >>= \case
    [] -> return Nothing
    (x:_) -> if isExceptionResume x then return (Just x)
               else return Nothing

-- |
--
isExceptionResume :: GHC.Resume -> Bool
isExceptionResume r = L.isInfixOf "exception" (GHC.resumeDecl r)



-- |
--
parseNameErrorHandler :: SomeException -> G.GHCi [GHC.Name]
parseNameErrorHandler e = liftIO $ print e >> return []


-- |
--
showTermErrorHandler :: SomeException -> G.GHCi SDoc
showTermErrorHandler e = return $ text $ show e


-- |
--
--
getEvalBody :: String -> Bool -> G.GHCi D.EvaluateBody
getEvalBody nameStr isForce =
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

    withTyThing (AnId i) = GHC.obtainTermFromId maxBound isForce i >>= withTerm i

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

      return D.defaultEvaluateBody {
               D.resultEvaluateBody = valStr
             , D.typeEvaluateBody   = typeStr
             }

    withTerm i _ = do
      dflags <- getDynFlags
      idSDoc <- pprTypeAndContents i
      let (_, typeStr, valStr) = getNameTypeValue (showSDoc dflags idSDoc)
      return D.defaultEvaluateBody {
               D.resultEvaluateBody = valStr
             , D.typeEvaluateBody  = typeStr
             }

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

