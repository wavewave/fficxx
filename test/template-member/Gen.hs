module Main where

import qualified Data.HashMap.Strict as HM (fromList)
import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Cabal (AddCInc(..),AddCSrc(..),Cabal(..),CabalName(..))
import FFICXX.Generate.Type.Config (ModuleUnit(..),ModuleUnitMap(..)
                                   ,ModuleUnitImports(..))
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface

-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------

-- import from stdcxx
stdcxx_cabal = Cabal { cabal_pkgname = CabalName "stdcxx"
                     , cabal_version = "0.5"
                     , cabal_cheaderprefix = "STD"
                     , cabal_moduleprefix = "STD"
                     , cabal_additional_c_incs = []
                     , cabal_additional_c_srcs = []
                     , cabal_additional_pkgdeps = []
                     , cabal_pkg_config_depends = []
                     }

-- import from stdcxx
deletable :: Class
deletable =
  AbstractClass stdcxx_cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing ]
  []
  []

-- import from stdcxx
string :: Class
string =
  Class stdcxx_cabal "string" [ ] mempty
  (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
  []
  []
  []

t_vector = TmplCls stdcxx_cabal "Vector" "std::vector" "t" [ ]




cabal_ testH testCpp =
  Cabal { cabal_pkgname = CabalName "testpkg"
        , cabal_version = "0.0"
        , cabal_cheaderprefix = "TestPkg"
        , cabal_moduleprefix = "TestPkg"
        , cabal_additional_c_incs = [
            AddCInc "test.h" testH
          ]
        , cabal_additional_c_srcs = [
            AddCSrc "test.cpp" testCpp
          ]
        , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
              , cabal_license = Just "BSD3"
        , cabal_licensefile = Just "LICENSE"
        , cabal_extraincludedirs = [ ]
        , cabal_extralibdirs = []
        , cabal_extrafiles = []
        , cabal_pkg_config_depends = []
        }

extraDep = [ ]

classA cabal =
  Class {
    class_cabal = cabal
  , class_name = "A"
  , class_parents = [ deletable ]
  , class_protected = mempty
  , class_alias = Nothing
  , class_funcs = [ Constructor [ ] Nothing ]
  , class_vars  = [ ]
  , class_tmpl_funcs = [ ]
  }

classT1 cabal =
  Class {
    class_cabal = cabal
  , class_name = "T1"
  , class_parents = [ deletable ]
  , class_protected = mempty
  , class_alias = Nothing
  , class_funcs = [ Constructor [ ] Nothing
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
  , class_funcs = [ Constructor [ ] Nothing
                  , NonVirtual void_ "print" [ ] Nothing
                  ]
  , class_vars = [ ]
  , class_tmpl_funcs = [ ]
  }

classes cabal = [ classA cabal, classT1 cabal, classT2 cabal ]

toplevelfunctions = [ ]

templates = [  ]

headerMap =
  ModuleUnitMap $
    HM.fromList $
      [ ( MU_Class "A"
        , ModuleUnitImports {
            muimports_namespaces = []
          , muimports_headers = [HdrName "test.h"]
          }
        )
      , ( MU_Class "T1"
        , ModuleUnitImports {
            muimports_namespaces = []
          , muimports_headers = [HdrName "test.h"]
          }
        )
      , ( MU_Class "T2"
        , ModuleUnitImports {
            muimports_namespaces = []
          , muimports_headers = [HdrName "test.h"]
          }
        )

      ]


main :: IO ()
main = do
  cabal <- do
    testH   <- readFile "test.h"
    testCpp <- readFile "test.cpp"
    pure (cabal_ testH testCpp)

  simpleBuilder
    "TestPkg"
    headerMap
    (cabal,classes cabal,toplevelfunctions,templates)
    [ ]
    extraDep
