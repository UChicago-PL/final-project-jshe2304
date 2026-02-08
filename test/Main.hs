module Main (main) where

import Test.Hspec
import qualified AutoDiffSpec

main :: IO ()
main = hspec $ do
    describe "AutoDiff" AutoDiffSpec.spec
