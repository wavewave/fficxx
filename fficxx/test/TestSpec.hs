module TestSpec ( spec ) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "test spec" $ do
    it "should pass" $
      1 `shouldBe` (2-1)
