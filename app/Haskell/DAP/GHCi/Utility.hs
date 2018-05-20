{-# LANGUAGE LambdaCase #-}

module Haskell.DAP.GHCi.Utility where

import qualified GHC
import qualified GHCi.UI.Monad as G
import Outputable
import Exception

import qualified Data.Char as CH
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Read as R
import Data.Maybe
import Data.Word

import Control.Monad.IO.Class
import Control.Concurrent

import Haskell.DAP.GHCi.Constant
import Haskell.DAP.GHCi.Type

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
--   phoityne -> haskell-dap
--   RequestArgument is encoded. decode to [Word8]
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
--   haskell-dap -> phoityne
--   Just show ResponseBody. no need to encode to [Word8]
--
showDAP :: Show a => a -> String
showDAP = show


-- |
--
printDAP :: Show a => a -> G.GHCi ()
printDAP dat = do
  let outStr = _DAP_HEADER ++ showDAP dat

  liftIO $ putStrLn outStr

-- |
--
printOutputEventDAP ::  (Either String D.OutputEventBody) -> G.GHCi ()
printOutputEventDAP dat = do
  let outStr = _DAP_HEADER_OUTPUT_EVENT ++ showDAP dat

  liftIO $ putStrLn outStr


-- |
--
clearTmpDAPContext :: G.GHCi ()
clearTmpDAPContext = do
  mvarCtx <- G.dapContextGHCiState  <$> G.getGHCiState 

  ctx <- liftIO $ takeMVar mvarCtx
  liftIO $ putMVar mvarCtx ctx{
      traceCmdExecResultDAPContext   = []
    , doContinueExecResultDAPContext = []
    , runStmtDeclExceptionDAPContext = []
    }


-- |
--
isExceptionResume :: GHC.Resume -> Bool
isExceptionResume (GHC.Resume{GHC.resumeBreakInfo = a}) = isNothing a


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
getRunStmtSourceError :: G.GHCi String
getRunStmtSourceError = do
  mvarCtx <- G.dapContextGHCiState <$> G.getGHCiState 

  ctx <- liftIO $ readMVar mvarCtx
  let errs = runStmtDeclExceptionDAPContext ctx
      msgs = "[DAP][ERROR] error occurred while runStmt." 
            : map show errs

  return $ L.intercalate "\n" msgs
    