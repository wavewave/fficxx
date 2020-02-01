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
                                      , Form(FormSimple)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevel(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Runtime.CodeGen.Cxx     ( Namespace(..), HeaderName(..) )



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
-- tmpl-dep-test
-- -------------------------------------------------------------------

cabal_ :: FilePath -> FilePath -> Cabal
cabal_ tdtestH tdtestCpp =
  Cabal { cabal_pkgname            = CabalName "tmpl-dep-test"
        , cabal_version            = "0.0"
        , cabal_cheaderprefix      = "TmplDepTest"
        , cabal_moduleprefix       = "TmplDepTest"
        , cabal_additional_c_incs  = [ AddCInc "tdtest.h" tdtestH ]
        , cabal_additional_c_srcs  = [ AddCSrc "tdtest.cpp" tdtestCpp ]
        , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
        , cabal_license            = Just "BSD3"
        , cabal_licensefile        = Just "LICENSE"
        , cabal_extraincludedirs   = []
        , cabal_extralibdirs       = []
        , cabal_extrafiles         = []
        , cabal_pkg_config_depends = []
        , cabal_buildType          = Simple
        }

extraDep :: [(String,[String])]
extraDep = []

extraLib :: [String]
extraLib = []

tT1 :: Cabal -> TemplateClass
tT1 cabal =
  TmplCls {
    tclass_cabal = cabal
  , tclass_name = "T1"
  , tclass_cxxform = FormSimple "T1"
  , tclass_params = [ "p1" ]
  , tclass_funcs = [
        TFunNew {
          tfun_new_args = []
        , tfun_new_alias = Nothing
        }
      , TFun {
          tfun_ret = Void
        , tfun_name = "method"
        , tfun_oname = "method"
        , tfun_args = []
        }
    ]
  , tclass_vars = []
  }

tT2 :: Cabal -> TemplateClass
tT2 cabal =
  TmplCls {
    tclass_cabal = cabal
  , tclass_name = "T2"
  , tclass_cxxform = FormSimple "T2"
  , tclass_params = [ "p1" ]
  , tclass_funcs = [
        TFunNew {
          tfun_new_args = []
        , tfun_new_alias = Nothing
        }
      , TFun {
          tfun_ret = Void
        , tfun_name = "callT1"
        , tfun_oname = "callT1"
        , tfun_args = [ Arg
                          (TemplateAppRef
                            TemplateAppInfo {
                              tapp_tclass = tT1 cabal
                            , tapp_tparams = [TArg_TypeParam "p1"]
                            , tapp_CppTypeForParam = "T1<p1>"
                            }
                          )
                          "tmpl1"
                      ]
        }
    ]
  , tclass_vars = []
  }

classes :: Cabal -> [Class]
classes cabal = []

toplevels :: [TopLevel]
toplevels = []

templates :: Cabal -> [TemplateClassImportHeader]
templates cabal =
  [ TCIH (tT1 cabal) ["tdtest.h"]
  , TCIH (tT2 cabal) ["tdtest.h"]
  ]

headers :: [(ModuleUnit, ModuleUnitImports)]
headers =
  []

main :: IO ()
main = do
  args <- getArgs
  let tmpldir =  if length args == 1
                 then args !! 0
                 else "../template"

  cwd <- getCurrentDirectory

  cabal <- do
    tdtestH   <- readFile (tmpldir </> "tdtest.h")
    tdtestCpp <- readFile (tmpldir </> "tdtest.cpp")
    pure (cabal_ tdtestH tdtestCpp)

  let fficfg = FFICXXConfig {
                 fficxxconfig_workingDir     = cwd </> "tmp" </> "working"
               , fficxxconfig_installBaseDir = cwd </> "tmpl-dep-test"
               , fficxxconfig_staticFileDir  = tmpldir
               }
      sbcfg  = SimpleBuilderConfig {
                 sbcTopModule  = "TmplDepTest"
               , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
               , sbcCabal      = cabal
               , sbcClasses    = classes cabal
               , sbcTopLevels  = toplevels
               , sbcTemplates  = templates cabal
               , sbcExtraLibs  = extraLib
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = []
               }

  simpleBuilder fficfg sbcfg
