{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GHCMain as G
import qualified GHCi.UI as G
import Control.Concurrent
import Haskell.DAP.GHCi.Command
import Haskell.DAP.GHCi.Type



-- |
--  Main
--
main :: IO ()
main = do
  putStrLn "[DAP][INFO] start haskell-dap."

  mvarCtx <- newMVar defaultDAPContext

  let defaultCommands = G.availableCommands G.defaultGhciSettings
      withDapCommands = defaultCommands ++ (dapCommands mvarCtx)

  G.ghcMain G.defaultGhciSettings {G.availableCommands = withDapCommands}
  