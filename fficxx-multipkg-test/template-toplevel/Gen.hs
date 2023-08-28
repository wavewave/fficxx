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
    Form (FormSimple),
    Function (..),
    ProtectedMethod (..),
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
import FFICXX.Runtime.Types (FFISafety (..))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

-- -------------------------------------------------------------------
-- stdcxx
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal =
  Cabal
    { cabal_pkgname = CabalName "stdcxx",
      cabal_version = "0.7.0.1",
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

t_vector :: TemplateClass
t_vector =
  TmplCls
    stdcxx_cabal
    "Vector"
    (FormSimple "std::vector")
    ["tp1"]
    [ TFunNew [] Nothing,
      TFun FFIUnsafe void_ "push_back" "push_back" [Arg (TemplateParam "tp1") "x"],
      TFun FFIUnsafe void_ "pop_back" "pop_back" [],
      TFun FFIUnsafe (TemplateParam "tp1") "at" "at" [int "n"],
      TFun FFIUnsafe int_ "size" "size" [],
      TFunDelete
    ]
    []

-- -------------------------------------------------------------------
-- tmpl-toplevel-test
-- -------------------------------------------------------------------

cabal_ :: FilePath -> FilePath -> Cabal
cabal_ tttestH tttestCpp =
  Cabal
    { cabal_pkgname = CabalName "tmpl-toplevel-test",
      cabal_version = "0.0",
      cabal_cheaderprefix = "TmplTopLevelTest",
      cabal_moduleprefix = "TmplTopLevelTest",
      cabal_additional_c_incs = [AddCInc "tttest.h" tttestH],
      cabal_additional_c_srcs = [AddCSrc "tttest.cpp" tttestCpp],
      cabal_additional_pkgdeps = [CabalName "stdcxx"],
      cabal_license = Just "BSD-3-Clause",
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

classes :: Cabal -> [Class]
classes cabal = []

toplevels :: [TopLevel]
toplevels =
  [ TLOrdinary
      TopLevelFunction
        { toplevelfunc_safety = FFIUnsafe,
          toplevelfunc_ret = Void,
          toplevelfunc_name = "ordinary",
          toplevelfunc_args = [],
          toplevelfunc_alias = Nothing
        },
    TLTemplate
      ( TopLevelTemplateFunction
          { topleveltfunc_safety = FFIUnsafe,
            topleveltfunc_params = ["t1"],
            topleveltfunc_ret =
              TemplateAppMove (TemplateAppInfo t_vector [TArg_TypeParam "t1"] "std::vector<t1>"),
            topleveltfunc_name = "return_vector",
            topleveltfunc_oname = "return_vector",
            topleveltfunc_args = [int "n"]
          }
      )
  ]

templates :: Cabal -> [TemplateClassImportHeader]
templates cabal = []

headers :: [(ModuleUnit, ModuleUnitImports)]
headers =
  [ ( MU_TopLevel,
      ModuleUnitImports
        { muimports_namespaces = [],
          muimports_headers = ["tttest.h"]
        }
    )
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
    tdtestH <- readFile (tmpldir </> "tttest.h")
    tdtestCpp <- readFile (tmpldir </> "tttest.cpp")
    pure (cabal_ tdtestH tdtestCpp)
  let fficfg =
        FFICXXConfig
          { fficxxconfig_workingDir = cwd </> "tmp" </> "working",
            fficxxconfig_installBaseDir = cwd </> "tmpl-toplevel-test",
            fficxxconfig_staticFileDir = tmpldir
          }
      sbcfg =
        SimpleBuilderConfig
          { sbcTopModule = "TmplTopLevelTest",
            sbcModUnitMap = ModuleUnitMap (HM.fromList headers),
            sbcCabal = cabal,
            sbcClasses = classes cabal,
            sbcEnums = [],
            sbcTopLevels = toplevels,
            sbcTemplates = templates cabal,
            sbcExtraLibs = extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = extraDep,
            sbcStaticFiles = []
          }
  simpleBuilder fficfg sbcfg
