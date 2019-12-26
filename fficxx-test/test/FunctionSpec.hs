{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module FunctionSpec ( spec ) where

import Foreign.Ptr
import System.IO.Silently (capture_)
--
import FFICXX.Runtime.TH
import FFICXX.Runtime.Function.Template
import qualified FFICXX.Runtime.Function.TH as TH
--
import Test.Hspec     ( Spec
                      , afterAll, anyException, around
                      , beforeAll, describe, it
                      , shouldBe, shouldThrow
                      )


TH.genFunctionInstanceFor
  [t|IO ()|]
  FPInfo {
    fpinfoCxxArgTypes = []
  , fpinfoCxxRetType = Nothing
  , fpinfoCxxHeaders =  []
  , fpinfoCxxNamespaces = []
  , fpinfoSuffix = "f1"
  }

TH.genFunctionInstanceFor
  [t|Int -> IO ()|]
  FPInfo {
    fpinfoCxxArgTypes = [ ("int","x") ]
  , fpinfoCxxRetType = Nothing
  , fpinfoCxxHeaders =  []
  , fpinfoCxxNamespaces = []
  , fpinfoSuffix = "f2"
  }

test :: IO ()
test = do
  putStrLn "haskell test"

closureTest :: Int -> IO ()
closureTest x = do
  putStrLn "closure test"
  putStrLn (" x = " ++ show x)

closureTest2 :: String -> Int -> IO ()
closureTest2 msg x = do
  putStrLn "closure test 2 "
  putStrLn msg
  putStrLn (" x = " ++ show x)

closureTest3 :: (Int -> Int) -> Int -> IO ()
closureTest3 f x = do
  putStrLn "closure test 3 "
  putStrLn (" f(x) = " ++ show (f x))

spec :: Spec
spec =
  describe "FFI to function" $ do
    describe "std::function<void()>" $ do
      it "should call a stored haskell function :: IO ()" $ do
        let action = do
              p_test1 <- wrapFunPtr test
              fptr1 <- newFunction p_test1
              call fptr1
              deleteFunction fptr1
        s <- capture_ action
        s `shouldBe` "haskell test\n"
      it "should call a stored haskell closure :: Int -> IO () with captured Int" $ do
        let action = do
              p_test1_1 <- wrapFunPtr (closureTest 32)
              fptr1_1 <- newFunction p_test1_1
              call fptr1_1
              deleteFunction fptr1_1
        s <- capture_ action
        s `shouldBe` "closure test\n x = 32\n"
    describe "std::function<void(int)>" $ do
      it "should call a stored haskell function :: Int -> IO ()" $ do
        let action = do
              p_test2 <- wrapFunPtr closureTest
              fptr2 <- newFunction p_test2
              call fptr2 26
              deleteFunction fptr2
        s <- capture_ action
        s `shouldBe` "closure test\n x = 26\n"
      it "should call a stored haskell closure :: String -> Int -> IO () with captured String" $ do
        let action = do
              p_test2_1 <- wrapFunPtr (closureTest2 "this is a captured message")
              fptr2_1 <- newFunction p_test2_1
              call fptr2_1 27
              deleteFunction fptr2_1
        s <- capture_ action
        s `shouldBe` "closure test 2 \nthis is a captured message\n x = 27\n"
      it "should call a stored haskell higher-order closure :: (Int -> Int) -> Int -> IO () with captured Int -> Int function" $ do
        let action = do
              p_test2_2 <- wrapFunPtr (closureTest3 (+100))
              fptr2_2 <- newFunction p_test2_2
              call fptr2_2 27
              deleteFunction fptr2_2
        s <- capture_ action
        s `shouldBe` "closure test 3 \n f(x) = 127\n"
