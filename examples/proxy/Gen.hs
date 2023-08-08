{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
--

import qualified Data.HashMap.Strict as HM (fromList)
import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Builder (simpleBuilder)
import FFICXX.Generate.Code.Primitive
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
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Type.Cabal
  ( AddCInc (..),
    AddCSrc (..),
    BuildType (..),
    Cabal (..),
    CabalName (..),
  )
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Class
  ( Arg (..),
    CTypes (CTDouble),
    Class (..),
    Function (..),
    ProtectedMethod (..),
    TLOrdinary (..),
    TopLevel (..),
    Variable (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
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
      cabal_version = "0.8",
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

-- -------------------------------------------------------------------
-- proxy-test
-- -------------------------------------------------------------------

cabal_ testH testCpp =
  Cabal
    { cabal_pkgname = CabalName "proxy-test",
      cabal_version = "0.0",
      cabal_cheaderprefix = "ProxyTest",
      cabal_moduleprefix = "ProxyTest",
      cabal_additional_c_incs = [AddCInc "test.h" testH],
      cabal_additional_c_srcs = [AddCSrc "test.cpp" testCpp],
      cabal_additional_pkgdeps = [CabalName "stdcxx"],
      cabal_license = Just "BSD-3-Clause",
      cabal_licensefile = Just "LICENSE",
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Simple
    }

extraDep = []

extraLib = []

impl cabal =
  Class
    { class_cabal = cabal,
      class_name = "Impl",
      class_parents = [deletable],
      class_protected = mempty,
      class_alias = Nothing,
      class_funcs = [Constructor [] Nothing],
      class_vars = [],
      class_tmpl_funcs = [],
      class_has_proxy = True
    }

loader cabal =
  Class
    { class_cabal = cabal,
      class_name = "Loader",
      class_parents = [deletable],
      class_protected = mempty,
      class_alias = Nothing,
      class_funcs =
        [ Constructor [cppclass (impl cabal) "m"] Nothing,
          NonVirtual void_ "invoke" [] Nothing
        ],
      class_vars = [],
      class_tmpl_funcs = [],
      class_has_proxy = False
    }

classes cabal = [impl cabal, loader cabal]

toplevelfunctions = []

templates = []

headers =
  [ modImports "Impl" [] ["test.h"],
    modImports "Loader" [] ["test.h"]
  ]

main :: IO ()
main = do
  args <- getArgs
  let tmpldir =
        if length args == 1
          then args !! 0
          else "./template"

  cwd <- getCurrentDirectory

  cabal <- do
    testH <- readFile (tmpldir </> "test.h")
    testCpp <- readFile (tmpldir </> "test.cpp")
    pure (cabal_ testH testCpp)

  let fficfg =
        FFICXXConfig
          { fficxxconfig_workingDir = cwd </> "tmp" </> "working2",
            fficxxconfig_installBaseDir = cwd </> "proxy-test",
            fficxxconfig_staticFileDir = tmpldir
          }
      sbcfg =
        SimpleBuilderConfig
          { sbcTopModule = "ProxyTest",
            sbcModUnitMap = ModuleUnitMap (HM.fromList headers),
            sbcCabal = cabal,
            sbcClasses = classes cabal,
            sbcTopLevels = toplevelfunctions,
            sbcTemplates = templates,
            sbcExtraLibs = extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = extraDep,
            sbcStaticFiles = []
          }

  simpleBuilder fficfg sbcfg
