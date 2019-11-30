{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.IORef                 ( IORef, newIORef, readIORef )
import Foreign.Ptr                ( castPtr )
--
import FFICXX.Runtime.Function.Template (Function(..),newFunction,wrapFunPtr)
--
import ProxyTest.Impl
import ProxyTest.Loader
--
import TH2 (newImplProxy)


testFn :: IORef Int -> IO ()
testFn ref = do
  putStrLn "Calling testFn in Haskell"
  n <- readIORef ref
  putStrLn ("n = " ++ show n)

main :: IO ()
main = do
  impl <- newImpl
  loader <- newLoader impl

  loader_invoke loader


  ref <- newIORef 8
  Function ptr <- newFunction =<< wrapFunPtr (testFn ref)

  implsub <- newImplProxy (castPtr ptr)
  loader2 <- newLoader implsub
  loader_invoke loader2

  pure ()
