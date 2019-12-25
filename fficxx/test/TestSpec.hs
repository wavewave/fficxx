module TestSpec ( spec ) where

import System.IO.Temp ( withSystemTempDirectory )
import System.Process.Typed ( readProcess_, proc )
--
import Test.Hspec     ( Spec, describe, it, shouldBe )

spec :: Spec
spec = do
  describe "test spec" $ do
    it "should pass" $
      withSystemTempDirectory "fficxx-cabal-sandbox" $ \dir -> do
        readProcess_ (proc "runhaskell" ["/home/ianwookim/repo/src/fficxx/stdcxx-gen/Gen.hs"])

        1 `shouldBe` (2-1)
