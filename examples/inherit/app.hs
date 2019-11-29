{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.IORef                 ( IORef, newIORef, readIORef )
import Foreign.Ptr                ( castPtr )
--
import FFICXX.Runtime.Function.Template (Function(..),newFunction,wrapFunPtr)
--
import InheritTest.Impl
-- import InheritTest.ImplSub
import InheritTest.Loader
--
import qualified TH

TH.genImplProxy

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

{-
  ref <- newIORef 8
  Function ptr <- newFunction =<< wrapFunPtr (testFn ref)

  implsub <- newImplSub (castPtr ptr)
  loader2 <- newLoader implsub
  loader_invoke loader2
-}
