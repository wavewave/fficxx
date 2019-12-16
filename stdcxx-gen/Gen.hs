{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import Data.Monoid                 ( mempty )
import System.Directory            ( getCurrentDirectory )
import System.FilePath             ( (</>) )
--
import FFICXX.Runtime.CodeGen.Cxx  ( HeaderName(..), Namespace(..) )
--
import FFICXX.Generate.Builder     ( simpleBuilder )
import FFICXX.Generate.Code.Primitive
                                   ( cppclassref, cppclassref_
                                   , cstring, cstring_
                                   , int, int_
                                   , void_
                                   )
import FFICXX.Generate.Config      ( FFICXXConfig(..)
                                   , SimpleBuilderConfig(..)
                                   )
import FFICXX.Generate.Type.Cabal  ( BuildType(Simple)
                                   , Cabal(..)
                                   , CabalName(..)
                                   )
import FFICXX.Generate.Type.Config ( ModuleUnitImports(..), ModuleUnitMap(..), ModuleUnit(..), modImports )
import FFICXX.Generate.Type.Class  ( Arg(..)
                                   , Class(..)
                                   , ClassAlias(..)
                                   , Function(..)
                                   , TemplateClass(..)
                                   , TemplateFunction(..)
                                   , TopLevelFunction
                                   , Types(..)
                                   )
import FFICXX.Generate.Type.Module ( TemplateClassImportHeader(..) )
import FFICXX.Generate.Type.PackageInterface ()


cabal :: Cabal
cabal = Cabal { cabal_pkgname = CabalName "stdcxx"
              , cabal_version = "0.6"
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
              , cabal_buildType = Simple
              }

extraDep :: [(String,[String])]
extraDep = [ ]


deletable :: Class
deletable =
  AbstractClass cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing ]
  []
  []

string :: Class
string =
  Class cabal "string" [ deletable ] mempty
    (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
    [ Constructor [ cstring "p" ] Nothing
    , NonVirtual cstring_ "c_str" [] Nothing
    , NonVirtual (cppclassref_ string) "append" [cppclassref string "str"] Nothing
    , NonVirtual (cppclassref_ string) "erase" [] Nothing
    ]
    []
    []
    False

ostream :: Class
ostream =
  Class cabal "ostream" [] mempty
    (Just (ClassAlias { caHaskellName = "Ostream", caFFIName = "ostream" }))
    []
    []
    []
    False

classes :: [Class]
classes = [ deletable
          --
          , ostream
          , string
          ]

toplevelfunctions :: [TopLevelFunction]
toplevelfunctions = [ ]

t_map :: TemplateClass
t_map =
  TmplCls cabal "Map" "std::map" ["k","v"]
    [ TFunNew [] Nothing
    , TFun int_  "size" "size" [] Nothing
    , TFunDelete
    ]

t_vector :: TemplateClass
t_vector =
  TmplCls cabal "Vector" "std::vector" ["t"]
    [ TFunNew [] Nothing
    , TFun void_ "push_back" "push_back" [Arg (TemplateParam "t") "x"] Nothing
    , TFun void_ "pop_back"  "pop_back"  []                        Nothing
    , TFun (TemplateParam "t") "at" "at" [int "n"]                 Nothing
    , TFun int_  "size"      "size"      []                        Nothing
    , TFunDelete
    ]

t_unique_ptr :: TemplateClass
t_unique_ptr =
  TmplCls cabal "UniquePtr" "std::unique_ptr" ["t"]
    [ TFunNew [] (Just "newUniquePtr0")
    , TFunNew [Arg (TemplateParamPointer "t") "p"] Nothing
    , TFun (TemplateParamPointer "t") "get" "get" [] Nothing
    , TFun (TemplateParamPointer "t") "release" "release" [] Nothing
    , TFun void_ "reset" "reset" [] Nothing
    , TFunDelete
    ]

t_shared_ptr :: TemplateClass
t_shared_ptr =
  TmplCls cabal "SharedPtr" "std::shared_ptr" ["t"]
    [ TFunNew [] (Just "newSharedPtr0")
    , TFunNew [Arg (TemplateParamPointer "t") "p"] Nothing
    , TFun (TemplateParamPointer "t") "get" "get" [] Nothing
    , TFun void_ "reset" "reset" [] Nothing
    , TFun int_ "use_count" "use_count" [] Nothing
    , TFunDelete
    ]

templates :: [TemplateClassImportHeader]
templates =
  [ TCIH t_map        "Map.h"       ["map"]
  , TCIH t_vector     "Vector.h"    ["vector"]
  , TCIH t_unique_ptr "UniquePtr.h" ["memory"]
  , TCIH t_shared_ptr "SharedPtr.h" ["memory"]
  ]

headers :: [ (ModuleUnit, ModuleUnitImports) ]
headers =
  [ modImports "string" ["std"] ["string"] ]

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let cfg = FFICXXConfig {
              fficxxconfig_workingDir = cwd </> "working"
            , fficxxconfig_installBaseDir = cwd </> "stdcxx"
            , fficxxconfig_staticFileDir = ""
            }
      sbc = SimpleBuilderConfig {
              sbcTopModule  = "STD"
            , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
            , sbcCabal      = cabal
            , sbcClasses    = classes
            , sbcTopLevels  = toplevelfunctions
            , sbcTemplates  = templates
            , sbcExtraLibs  = ["stdc++"]
            , sbcExtraDeps  = extraDep
            , sbcStaticFiles = []
            }
  simpleBuilder cfg sbc
