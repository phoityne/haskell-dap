{-# LANGUAGE OverloadedStrings #-}

module Haskell.DAP.ControlSpec where

import Test.Hspec
import Data.Default

spec :: Spec
spec = 
  describe "Control test" $ 
    context "with default" $ 
      it "should throw exception" $ do
        pendingWith "spec is pending."



