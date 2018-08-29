{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr

import Binding

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




main = do
  putStrLn "hsMain"

  putStrLn "-- test 1 ----------"
  p_test1 <- create_f1 test
  Function fptr1 <- c_Function_new_f1 p_test1
  c_Function_call_f1 fptr1

  putStrLn "-- test 1-1 --------"
  p_test1_1 <- create_f1 (closureTest 32)
  Function fptr1_1 <- c_Function_new_f1 p_test1_1
  c_Function_call_f1 fptr1_1


  putStrLn "-- test 2 ----------"
  p_test2 <- create_f2 closureTest
  Function fptr2 <- c_Function_new_f2 p_test2
  c_Function_call_f2 fptr2 26

  putStrLn "-- test 2-1 --------"
  p_test2_1 <- create_f2 (closureTest2 "this is a captured message")
  Function fptr2_1 <- c_Function_new_f2 p_test2_1
  c_Function_call_f2 fptr2_1 27

  pure ()
