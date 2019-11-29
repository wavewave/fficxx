{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import FFICXX.Runtime.TH ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.UniquePtr.Template
import STD.UniquePtr.TH

import InheritTest.Impl
import InheritTest.ImplSub
import InheritTest.Loader


main :: IO ()
main = do
  impl <- newImpl
  loader <- newLoader impl

  loader_invoke loader


  implsub <- newImplSub
  loader2 <- newLoader implsub
  loader_invoke loader2
