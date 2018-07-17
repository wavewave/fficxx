module Main where

import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = "stdcxx"
              , cabal_cheaderprefix = "STD"
              , cabal_moduleprefix = "STD"
              , cabal_additional_c_incs = []
              , cabal_additional_c_srcs = []
              , cabal_additional_pkgdeps = []
              }

extraDep = [ ]

cabalattr =
    CabalAttr
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = [ ]
    , cabalattr_extralibdirs = []
    , cabalattr_extrafiles = []
    }


deletable :: Class
deletable =
  AbstractClass cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing
  ]

string :: Class
string =
  Class cabal "string" [ deletable ] mempty  (Just "CppString")
  [ Constructor [ cstring "p" ] Nothing
  , NonVirtual cstring_ "c_str" [] Nothing
  , NonVirtual (cppclassref_ string) "append" [cppclassref string "str"] Nothing
  , NonVirtual (cppclassref_ string) "erase" [] Nothing
  ]

ostream :: Class
ostream = Class cabal "ostream" [] mempty (Just "Ostream") []


classes = [ deletable
          --
          , ostream
          , string
          ]

toplevelfunctions = [ ]

templates = [  ]

headerMap = [ ("string"         , ([NS "std"          ], [HdrName "string"   ]))
            ]

main :: IO ()
main = do
  simpleBuilder
    "STD"
    headerMap
    (cabal,cabalattr,classes,toplevelfunctions,templates)
    [ ]
    extraDep
