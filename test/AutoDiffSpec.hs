module AutoDiffSpec (spec) where

import Test.Hspec
import AutoDiff

spec :: Spec
spec = do
    describe "diff" $ do
        it "differentiates x^2 to 2x" $ do
            diff (\x -> x * x) 3 `shouldBe` 6

        it "differentiates x^3 to 3x^2" $ do
            diff (\x -> x * x * x) 2 `shouldBe` 12

        it "differentiates constants to 0" $ do
            diff (const (constant 5)) 10 `shouldBe` 0

    describe "Dual arithmetic" $ do
        it "adds dual numbers correctly" $ do
            var 3 + var 4 `shouldBe` Dual 7 2

        it "multiplies dual numbers correctly" $ do
            var 3 * constant 2 `shouldBe` Dual 6 2
