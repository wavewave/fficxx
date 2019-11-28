{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Foreign.Ptr

import FFICXX.Runtime.TH
import FFICXX.Runtime.Function.Template
import qualified FFICXX.Runtime.Function.TH as TH

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


main :: IO ()
main = do
  putStrLn "hsMain"

  putStrLn "-- test 1 ----------"
  p_test1 <- wrapFunPtr test
  fptr1 <- newFunction p_test1
  call fptr1
  deleteFunction fptr1

  putStrLn "-- test 1-1 --------"
  p_test1_1 <- wrapFunPtr (closureTest 32)
  fptr1_1 <- newFunction p_test1_1
  call fptr1_1
  deleteFunction fptr1_1

  putStrLn "-- test 2 ----------"
  p_test2 <- wrapFunPtr closureTest
  fptr2 <- newFunction p_test2
  call fptr2 26
  deleteFunction fptr2

  putStrLn "-- test 2-1 --------"
  p_test2_1 <- wrapFunPtr (closureTest2 "this is a captured message")
  fptr2_1 <- newFunction p_test2_1
  call fptr2_1 27
  deleteFunction fptr2_1

  putStrLn "-- test 2-2 --------"
  p_test2_2 <- wrapFunPtr (closureTest3 (+100))
  fptr2_2 <- newFunction p_test2_2
  call fptr2_2 27
  deleteFunction fptr2_2

  pure ()
