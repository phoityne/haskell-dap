{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GHCMain as G
import qualified GHCi.UI as G
import Control.Concurrent
import qualified Data.Map as M
import Haskell.DAP.GHCi.Command



-- |
--  Main
--
main :: IO ()
main = do
  putStrLn "[INFO] start haskell-dap."

  let ctx =  DAPContext {
             variableReferenceMapDAPContext = M.fromList []
          }

  mvarCtx <- newMVar ctx

  let defaultCommands = G.availableCommands G.defaultGhciSettings
      withDapCommands = defaultCommands ++ (dapCommands mvarCtx)

  G.ghcMain G.defaultGhciSettings {G.availableCommands = withDapCommands}
  