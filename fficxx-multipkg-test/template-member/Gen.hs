{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
--

import qualified Data.HashMap.Strict as HM (fromList)
import Data.Monoid (mempty)
import FFICXX.Generate.Builder (simpleBuilder)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
  ( bool_,
    charpp,
    cppclass,
    cppclass_,
    cstring,
    cstring_,
    double,
    double_,
    int,
    int_,
    uint,
    uint_,
    void_,
    voidp,
  )
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Type.Cabal (BuildType (..), Cabal (..), CabalName (..))
import FFICXX.Generate.Type.Cabal
  ( AddCInc (..),
    AddCSrc (..),
    BuildType (Simple),
    Cabal (..),
    CabalName (..),
  )
import FFICXX.Generate.Type.Class
  ( Arg (..),
    CTypes (CTDouble),
    Class (..),
    Form (FormSimple),
    Function (..),
    ProtectedMethod (..),
    TopLevel (..),
    Variable (..),
  )
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
  )
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal =
  Cabal
    { cabal_pkgname = CabalName "stdcxx",
      cabal_version = "0.6",
      cabal_cheaderprefix = "STD",
      cabal_moduleprefix = "STD",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [],
      cabal_license = Nothing,
      cabal_licensefile = Nothing,
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Simple
    }

-- import from stdcxx
deletable :: Class
deletable =
  AbstractClass
    { class_cabal = stdcxx_cabal,
      class_name = "Deletable",
      class_parents = [],
      class_protected = Protected [],
      class_alias = Nothing,
      class_funcs = [Destructor Nothing],
      class_vars = [],
      class_tmpl_funcs = []
    }

-- import from stdcxx
string :: Class
string =
  Class
    stdcxx_cabal
    "string"
    [deletable]
    mempty
    (Just (ClassAlias {caHaskellName = "CppString", caFFIName = "string"}))
    [ Constructor [cstring "p"] Nothing,
      NonVirtual cstring_ "c_str" [] Nothing,
      NonVirtual (cppclassref_ string) "append" [cppclassref string "str"] Nothing,
      NonVirtual (cppclassref_ string) "erase" [] Nothing
    ]
    []
    []
    False

t_vector :: TemplateClass
t_vector =
  TmplCls
    stdcxx_cabal
    "Vector"
    (FormSimple "std::vector")
    ["tp1"]
    [ TFunNew [] Nothing,
      TFun void_ "push_back" "push_back" [Arg (TemplateParam "tp1") "x"],
      TFun void_ "pop_back" "pop_back" [],
      TFun (TemplateParam "tp1") "at" "at" [int "n"],
      TFun int_ "size" "size" [],
      TFunDelete
    ]
    []

t_unique_ptr :: TemplateClass
t_unique_ptr =
  TmplCls
    stdcxx_cabal
    "UniquePtr"
    (FormSimple "std::unique_ptr")
    ["tp1"]
    [ TFunNew [] (Just "newUniquePtr0"),
      TFunNew [Arg (TemplateParamPointer "tp1") "p"] Nothing,
      TFun (TemplateParamPointer "tp1") "get" "get" [],
      TFun (TemplateParamPointer "tp1") "release" "release" [],
      TFun void_ "reset" "reset" [],
      TFunDelete
    ]
    []

-- -------------------------------------------------------------------
-- tmf-test
-- -------------------------------------------------------------------

cabal_ :: FilePath -> FilePath -> Cabal
cabal_ tmftestH tmftestCpp =
  Cabal
    { cabal_pkgname = CabalName "tmf-test",
      cabal_version = "0.0",
      cabal_cheaderprefix = "TMFTest",
      cabal_moduleprefix = "TMFTest",
      cabal_additional_c_incs = [AddCInc "tmftest.h" tmftestH],
      cabal_additional_c_srcs = [AddCSrc "tmftest.cpp" tmftestCpp],
      cabal_additional_pkgdeps = [CabalName "stdcxx"],
      cabal_license = Just "BSD3",
      cabal_licensefile = Just "LICENSE",
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Simple
    }

extraDep :: [(String, [String])]
extraDep = []

extraLib :: [String]
extraLib = []

classA :: Cabal -> Class
classA cabal =
  Class
    { class_cabal = cabal,
      class_name = "A",
      class_parents = [deletable],
      class_protected = mempty,
      class_alias = Nothing,
      class_funcs = [Constructor [] Nothing],
      class_vars = [],
      class_tmpl_funcs =
        [ TemplateMemberFunction
            { tmf_params = ["tp1"],
              tmf_ret = void_,
              tmf_name = "method",
              tmf_args = [Arg (TemplateParamPointer "tp1") "x"],
              tmf_alias = Nothing
            },
          TemplateMemberFunction
            { tmf_params = ["tp1"],
              tmf_ret = void_,
              tmf_name = "method2",
              tmf_args =
                [ Arg
                    ( TemplateAppMove
                        TemplateAppInfo
                          { tapp_tclass = t_unique_ptr,
                            tapp_tparams = [TArg_TypeParam "tp1"],
                            tapp_CppTypeForParam = "std::unique_ptr<tp1>"
                          }
                    )
                    "x"
                ],
              tmf_alias = Nothing
            }
        ],
      class_has_proxy = False
    }

classT1 :: Cabal -> Class
classT1 cabal =
  Class
    { class_cabal = cabal,
      class_name = "T1",
      class_parents = [deletable],
      class_protected = mempty,
      class_alias = Nothing,
      class_funcs =
        [ Constructor [] Nothing,
          NonVirtual void_ "print" [] Nothing
        ],
      class_vars = [],
      class_tmpl_funcs = [],
      class_has_proxy = False
    }

classT2 :: Cabal -> Class
classT2 cabal =
  Class
    { class_cabal = cabal,
      class_name = "T2",
      class_parents = [deletable],
      class_protected = mempty,
      class_alias = Nothing,
      class_funcs =
        [ Constructor [] Nothing,
          NonVirtual void_ "print" [] Nothing
        ],
      class_vars = [],
      class_tmpl_funcs = [],
      class_has_proxy = False
    }

classes :: Cabal -> [Class]
classes cabal = [classA cabal, classT1 cabal, classT2 cabal]

toplevels :: [TopLevel]
toplevels = []

templates :: [TemplateClassImportHeader]
templates = []

headers :: [(ModuleUnit, ModuleUnitImports)]
headers =
  [ modImports "A" [] ["tmftest.h"],
    modImports "T1" [] ["tmftest.h"],
    modImports "T2" [] ["tmftest.h"]
  ]

main :: IO ()
main = do
  args <- getArgs
  let tmpldir =
        if length args == 1
          then args !! 0
          else "../template"
  cwd <- getCurrentDirectory
  cabal <- do
    tmftestH <- readFile (tmpldir </> "tmftest.h")
    tmftestCpp <- readFile (tmpldir </> "tmftest.cpp")
    pure (cabal_ tmftestH tmftestCpp)
  let fficfg =
        FFICXXConfig
          { fficxxconfig_workingDir = cwd </> "tmp" </> "working",
            fficxxconfig_installBaseDir = cwd </> "tmf-test",
            fficxxconfig_staticFileDir = tmpldir
          }
      sbcfg =
        SimpleBuilderConfig
          { sbcTopModule = "TMFTest",
            sbcModUnitMap = ModuleUnitMap (HM.fromList headers),
            sbcCabal = cabal,
            sbcClasses = classes cabal,
            sbcTopLevels = toplevels,
            sbcTemplates = templates,
            sbcExtraLibs = extraLib,
            sbcExtraDeps = extraDep,
            sbcStaticFiles = []
          }
  simpleBuilder fficfg sbcfg
