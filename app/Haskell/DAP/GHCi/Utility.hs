module Haskell.DAP.GHCi.Utility where

import qualified Data.Char as CH

import qualified GHC

import qualified GHCi.UI.Monad as G

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Text.Read as R
import Data.Word

import Control.Monad.IO.Class
import System.Console.Haskeline

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
  liftIO $ putStrLn outStr
  
showStoppedEventBody (Just (GHC.ExecComplete { GHC.execResult = Left (SomeException e)})) = do
  let body = Right D.defaultStoppedEventBody {
             D.reasonStoppedEventBody = "exception"
           , D.descriptionStoppedEventBody = show e
           } :: Either String D.StoppedEventBody
      outStr = _DAP_HEADER ++ showDAP body
  liftIO $ putStrLn outStr

showStoppedEventBody z@(Just GHC.ExecBreak{}) = do
  let body = Right D.defaultStoppedEventBody {
             D.reasonStoppedEventBody = "step"
           } :: Either String D.StoppedEventBody
      outStr = _DAP_HEADER ++ showDAP body
  liftIO $ print z
  liftIO $ putStrLn outStr

