{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import FFICXX.Runtime.CodeGen.Cxx ( HeaderName(..), Namespace(..) )
import FFICXX.Runtime.TH          ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.CppString
import STD.Map.Template
import qualified STD.Map.TH as TH


TH.genMapInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )
  ( [t|CDouble|], TPInfo { tpinfoCxxType       = "double"
                         , tpinfoCxxHeaders    = []
                         , tpinfoCxxNamespaces = []
                         , tpinfoSuffix        = "double"
                         }
  )


test1 :: IO ()
test1 = do
  m :: Map CInt CDouble <- newMap
  pure ()

main :: IO ()
main = do
  test1
