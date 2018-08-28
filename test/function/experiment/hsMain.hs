{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.Ptr

import Binding

foreign import ccall "wrapper" createIO :: IO () -> IO (FunPtr (IO ()))


test :: IO ()
test = do
  putStrLn "haskell test"


main = do
  putStrLn "hsMain"
  p_test <- createIO test
  Function fptr <- c_Function_new_f1 p_test

  c_Function_call_f1 fptr

  pure ()
