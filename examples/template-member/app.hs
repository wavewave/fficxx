{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import FFICXX.Runtime.TH ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.UniquePtr.Template
import STD.UniquePtr.TH

import TMFTest.A
import TMFTest.A.Implementation
import TMFTest.T1
import TMFTest.T2

genUniquePtrInstanceFor
  NonCPrim
  ( [t|T1|], TPInfo { tpinfoCxxType       = "T1"
                    , tpinfoCxxHeaders    = [ "TMFTestT1.h", "test.h" ]
                    , tpinfoCxxNamespaces = []
                    , tpinfoSuffix        = "T1"
                    }
  )

genInstanceFor_a_method
  NonCPrim
  ( [t|T1|], TPInfo { tpinfoCxxType       = "T1"
                    , tpinfoCxxHeaders    = [ "TMFTestT1.h" ]
                    , tpinfoCxxNamespaces = []
                    , tpinfoSuffix        = "T1"
                    }
  )

genInstanceFor_a_method
  NonCPrim
  ( [t|T2|], TPInfo { tpinfoCxxType       = "T2"
                    , tpinfoCxxHeaders    = [ "TMFTestT2.h" ]
                    , tpinfoCxxNamespaces = []
                    , tpinfoSuffix        = "T2"
                    }
  )

genInstanceFor_a_method2
  NonCPrim
  ( [t|T1|], TPInfo { tpinfoCxxType       = "T1"
                    , tpinfoCxxHeaders    = [ "TMFTestT1.h" ]
                    , tpinfoCxxNamespaces = []
                    , tpinfoSuffix        = "T1"
                    }
  )

main :: IO ()
main = do
  a <- newA
  t1 <- newT1
  t1_print t1
  t2 <- newT2
  a_method_T1 a t1
  a_method_T2 a t2

  ptr <- newUniquePtr t1
  a_method2_T1 a ptr
  pure ()
