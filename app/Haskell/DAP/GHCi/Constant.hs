module Haskell.DAP.GHCi.Constant where

  
-- |
--
_DAP_HEADER :: String
_DAP_HEADER = "<<DAP>>"


-- |
--
_DAP_CMD_END :: String
_DAP_CMD_END = "<<DAP_CMD_END>>"

-- |
--
_DAP_HEADER_OUTPUT_EVENT :: String
_DAP_HEADER_OUTPUT_EVENT = "<<DAP_OUTPUT_EVENT>>"

-- |
--
_GHCi_SCOPE :: String
_GHCi_SCOPE = "GHCi Local"
    
-- |
--
_GHCi_GLOBAL_SCOPE :: String
_GHCi_GLOBAL_SCOPE = "GHCi Global"
  
-- |
--
_BINDING_INSPECT_DEPTH :: Int
_BINDING_INSPECT_DEPTH = 100

-- |
--
_EVALUATE_INSPECT_DEPTH :: Int
_EVALUATE_INSPECT_DEPTH = 1000