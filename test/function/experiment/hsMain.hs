{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr

import Binding

foreign import ccall "wrapper" createIO :: IO () -> IO (FunPtr (IO ()))


test :: IO ()
test = do
  putStrLn "haskell test"


closureTest :: Int -> IO ()
closureTest x = do
  putStrLn "closure test"
  putStrLn (" x = " ++ show x)

main = do
  putStrLn "hsMain"

  putStrLn "------------"
  p_test1 <- createIO test
  Function fptr1 <- c_Function_new_f1 p_test1
  c_Function_call_f1 fptr1

  putStrLn "------------"
  p_test2 <- createIO (closureTest 32)
  Function fptr2 <- c_Function_new_f1 p_test2
  c_Function_call_f1 fptr2




  pure ()
