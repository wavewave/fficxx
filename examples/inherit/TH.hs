{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH where

import FFICXX.Runtime.TH          ( FunctionParamInfo(..), IsCPrimitive(..), TemplateParamInfo(..) )
import FFICXX.Runtime.Function.Template
import FFICXX.Runtime.Function.TH ( genFunctionInstanceFor )

genFunctionInstanceFor
  [t|IO ()|]
  FPInfo {
    fpinfoCxxArgTypes = []
  , fpinfoCxxRetType = Nothing
  , fpinfoCxxHeaders =  []
  , fpinfoCxxNamespaces = []
  , fpinfoSuffix = "f1"
  }
