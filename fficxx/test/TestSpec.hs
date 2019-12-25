module TestSpec ( spec ) where

import System.IO.Temp ( withSystemTempDirectory )
--
-- import qualified Gen  ( main )
--
import Test.Hspec     ( Spec, describe, it, shouldBe )

spec :: Spec
spec = do
  describe "test spec" $ do
    it "should pass" $
      withSystemTempDirectory "fficxx-cabal-sandbox" $ \dir -> do
        -- Gen.main

        1 `shouldBe` (2-1)
