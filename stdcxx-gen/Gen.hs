module Main where

import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Cabal (Cabal(..),CabalName(..))
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = CabalName "stdcxx"
              , cabal_cheaderprefix = "STD"
              , cabal_moduleprefix = "STD"
              , cabal_additional_c_incs = []
              , cabal_additional_c_srcs = []
              , cabal_additional_pkgdeps = []
              , cabal_license = Just "BSD3"
              , cabal_licensefile = Just "LICENSE"
              , cabal_extraincludedirs = [ ]
              , cabal_extralibdirs = []
              , cabal_extrafiles = []
              , cabal_pkg_config_depends = []
              }

extraDep = [ ]


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

t_vector = TmplCls cabal "Vector" "std::vector" "t"
             [ TFunNew []
             , TFun void_ "push_back" "push_back" [(TemplateParam "t","x")] Nothing
             , TFun void_ "pop_back"  "pop_back"  []                        Nothing
             , TFun (TemplateParam "t") "at" "at" [int "n"]                 Nothing
             , TFun int_  "size"      "size"      []                        Nothing
             , TFunDelete
             ]

templates = [ (t_vector, HdrName "Vector.h") ]


headerMap = [ ("string"         , ([NS "std"          ], [HdrName "string"   ]))
            ]

main :: IO ()
main = do
  simpleBuilder
    "STD"
    headerMap
    (cabal,classes,toplevelfunctions,templates)
    [ ]
    extraDep
