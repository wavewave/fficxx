{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Foreign.Ptr

-- import Binding
import Template
import TH

data Test = Test

$(TH.genFunctionInstanceFor [t|IO ()|] "f1" )
$(TH.genFunctionInstanceFor [t|Int -> IO ()|] "f2")

foreign import ccall "wrapper" create_f1 :: IO () -> IO (FunPtr (IO ()))

foreign import ccall "wrapper" create_f2 :: (Int -> IO ()) -> IO (FunPtr (Int -> IO ()))


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




main = do
  putStrLn "hsMain"

  putStrLn "-- test 1 ----------"
  p_test1 <- create_f1 test
  fptr1 <- newFunction p_test1
  -- fptr1 <- c_Function_new_f1 p_test1


  -- c_Function_call_f1 fptr1

  putStrLn "-- test 1-1 --------"
  p_test1_1 <- create_f1 (closureTest 32)
  fptr1_1 <- newFunction p_test1_1
  -- c_Function_call_f1 fptr1_1


  putStrLn "-- test 2 ----------"
  p_test2 <- create_f2 closureTest
  fptr2 <- newFunction p_test2
  -- c_Function_call_f2 fptr2 26

  putStrLn "-- test 2-1 --------"
  p_test2_1 <- create_f2 (closureTest2 "this is a captured message")
  fptr2_1 <- newFunction p_test2_1
  -- c_Function_call_f2 fptr2_1 27

  putStrLn "-- test 2-2 --------"
  p_test2_2 <- create_f2 (closureTest3 (+100))
  fptr2_2 <- newFunction p_test2_2
  -- c_Function_call_f2 fptr2_2 27

  pure ()
