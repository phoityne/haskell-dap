{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ >= 808
import Haskell.DAP.Constant

-- |
--  MainDeprecated
--
main :: IO ()
main = do
  putStrLn _DEPRECATED_MSG
  return ()

#endif

#if __GLASGOW_HASKELL__ < 808
import qualified GHCMain as G
import qualified GHCi.UI as G
import Control.Concurrent
import Haskell.DAP.GHCi.Command
import Haskell.DAP.GHCi.Type

import Paths_haskell_dap (version)
import Data.Version

-- |
--  Main
--
main :: IO ()
main = do
  putStrLn $ "[DAP][INFO] start haskell-dap-" ++ showVersion version ++ "."

  mvarCtx <- newMVar defaultDAPContext

  let ghciSettings    = G.defaultGhciSettings mvarCtx
      defaultCommands = G.availableCommands ghciSettings
      withDapCommands = defaultCommands ++ (dapCommands mvarCtx)

  G.ghcMain ghciSettings {G.availableCommands = withDapCommands}

#endif
