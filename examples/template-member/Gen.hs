{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
--
import FFICXX.Generate.Builder        ( simpleBuilder )
import FFICXX.Generate.Code.Primitive ( bool_
                                      , charpp
                                      , cppclass, cppclass_
                                      , cstring, cstring_
                                      , double, double_
                                      , int, int_
                                      , uint, uint_
                                      , void_, voidp
                                      )
import FFICXX.Generate.Config         ( FFICXXConfig(..)
                                      , SimpleBuilderConfig(..)
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..)
                                      , modImports
                                      )
import FFICXX.Generate.Type.Class     ( Arg(..)
                                      , Class(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Runtime.CodeGen.C       ( Namespace(..), HeaderName(..) )



import qualified Data.HashMap.Strict as HM (fromList)
import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Cabal     ( AddCInc(..)
                                      , AddCSrc(..)
                                      , BuildType(Simple)
                                      , Cabal(..)
                                      , CabalName(..)
                                      )
import FFICXX.Generate.Type.Config (ModuleUnit(..),ModuleUnitMap(..)
                                   ,ModuleUnitImports(..))
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface

-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal = Cabal {
    cabal_pkgname            = CabalName "stdcxx"
  , cabal_version            = "0.6"
  , cabal_cheaderprefix      = "STD"
  , cabal_moduleprefix       = "STD"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Simple
  }

-- import from stdcxx
deletable :: Class
deletable =
  AbstractClass {
    class_cabal      = stdcxx_cabal
  , class_name       = "Deletable"
  , class_parents    = []
  , class_protected  = Protected []
  , class_alias      = Nothing
  , class_funcs      = [ Destructor Nothing ]
  , class_vars       = []
  , class_tmpl_funcs = []
  }

-- import from stdcxx
string :: Class
string =
  Class stdcxx_cabal "string" [ deletable ] mempty
    (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
    [ Constructor [ cstring "p" ] Nothing
    , NonVirtual cstring_ "c_str" [] Nothing
    , NonVirtual (cppclassref_ string) "append" [cppclassref string "str"] Nothing
    , NonVirtual (cppclassref_ string) "erase" [] Nothing
    ]
    []
    []


t_vector :: TemplateClass
t_vector = TmplCls stdcxx_cabal "Vector" "std::vector" "t"
             [ TFunNew [] Nothing
             , TFun void_ "push_back" "push_back" [Arg (TemplateParam "t") "x"] Nothing
             , TFun void_ "pop_back"  "pop_back"  []                        Nothing
             , TFun (TemplateParam "t") "at" "at" [int "n"]                 Nothing
             , TFun int_  "size"      "size"      []                        Nothing
             , TFunDelete
             ]

t_unique_ptr :: TemplateClass
t_unique_ptr = TmplCls stdcxx_cabal "UniquePtr" "std::unique_ptr" "t"
             [ TFunNew [] (Just "newUniquePtr0")
             , TFunNew [Arg (TemplateParamPointer "t") "p"] Nothing
             , TFun (TemplateParamPointer "t") "get" "get" [] Nothing
             , TFun (TemplateParamPointer "t") "release" "release" [] Nothing
             , TFun void_ "reset" "reset" [] Nothing
             , TFunDelete
             ]

-- -------------------------------------------------------------------
-- tmf-test
-- -------------------------------------------------------------------




cabal_ testH testCpp =
  Cabal { cabal_pkgname            = CabalName "tmf-test"
        , cabal_version            = "0.0"
        , cabal_cheaderprefix      = "TMFTest"
        , cabal_moduleprefix       = "TMFTest"
        , cabal_additional_c_incs  = [ AddCInc "test.h" testH ]
        , cabal_additional_c_srcs  = [ AddCSrc "test.cpp" testCpp ]
        , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
        , cabal_license            = Just "BSD3"
        , cabal_licensefile        = Just "LICENSE"
        , cabal_extraincludedirs   = []
        , cabal_extralibdirs       = []
        , cabal_extrafiles         = []
        , cabal_pkg_config_depends = []
        , cabal_buildType          = Simple
        }

extraDep = []

extraLib = []

classA cabal =
  Class {
    class_cabal = cabal
  , class_name = "A"
  , class_parents = [ deletable ]
  , class_protected = mempty
  , class_alias = Nothing
  , class_funcs = [ Constructor [ ] Nothing ]
  , class_vars  = [ ]
  , class_tmpl_funcs =
      [ TemplateMemberFunction {
          tmf_param = "t"
        , tmf_ret = void_
        , tmf_name = "method"
        , tmf_args = [ Arg (TemplateParamPointer "t") "x" ]
        , tmf_alias = Nothing
        }
      , TemplateMemberFunction {
          tmf_param = "t"
        , tmf_ret = void_
        , tmf_name = "method2"
        , tmf_args = [ Arg
                         (TemplateAppMove
                           TemplateAppInfo {
                             tapp_tclass = t_unique_ptr
                           , tapp_tparam = TArg_TypeParam "t"
                           , tapp_CppTypeForParam = "std::unique_ptr<Type>"
                           }
                         )
                         "x"
                     ]
        , tmf_alias = Nothing
        }
      ]
  }

classT1 cabal =
  Class {
    class_cabal = cabal
  , class_name = "T1"
  , class_parents = [ deletable ]
  , class_protected = mempty
  , class_alias = Nothing
  , class_funcs =
      [ Constructor [ ] Nothing
      , NonVirtual void_ "print" [ ] Nothing
      ]
  , class_vars = [ ]
  , class_tmpl_funcs = [ ]
  }

classT2 cabal =
  Class {
    class_cabal = cabal
  , class_name = "T2"
  , class_parents = [ deletable ]
  , class_protected = mempty
  , class_alias = Nothing
  , class_funcs =
      [ Constructor [ ] Nothing
      , NonVirtual void_ "print" [ ] Nothing
      ]
  , class_vars = [ ]
  , class_tmpl_funcs = [ ]
  }

classes cabal = [ classA cabal, classT1 cabal, classT2 cabal ]

toplevelfunctions = [ ]

templates = [  ]

headers =
  [ modImports "A"  [] ["test.h"]
  , modImports "T1" [] ["test.h"]
  , modImports "T2" [] ["test.h"]
  ]

main :: IO ()
main = do
  args <- getArgs
  let tmpldir =  if length args == 1
                 then args !! 0
                 else "../template"

  cwd <- getCurrentDirectory

  cabal <- do
    testH   <- readFile (tmpldir </> "test.h")
    testCpp <- readFile (tmpldir </> "test.cpp")
    pure (cabal_ testH testCpp)

  let fficfg = FFICXXConfig {
                 fficxxconfig_workingDir     = cwd </> "tmp" </> "working"
               , fficxxconfig_installBaseDir = cwd </> "tmf-test"
               , fficxxconfig_staticFileDir  = tmpldir
               }
      sbcfg  = SimpleBuilderConfig {
                 sbcTopModule  = "TMFTest"
               , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
               , sbcCabal      = cabal
               , sbcClasses    = classes cabal
               , sbcTopLevels  = toplevelfunctions
               , sbcTemplates  = templates
               , sbcExtraLibs  = extraLib
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = []
               }

  simpleBuilder fficfg sbcfg

{-
  simpleBuilder
    "TestPkg"
    headerMap
    (cabal,classes cabal,toplevelfunctions,templates)
    [ ]
    extraDep
-}
