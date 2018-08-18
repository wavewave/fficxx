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
  [ Destructor Nothing
  ]


-- import from stdcxx
string :: Class
string =
  Class stdcxx_cabal "string" [ ] mempty
  (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "CppString"}))
  [ ]

t_vector = TmplCls stdcxx_cabal "Vector" "std::vector" "t" [ ]

testH =
  "#include <iostream>\n\
  \#include <vector>\n\
  \void test( std::vector<float>&  vect);\n\
  \class A {\n\
  \public:\n\
  \  A() {}\n\
  \  ~A() {\n\
  \     std::cout << \"A deleted\" << std::endl;\n\
  \  }\n\
  \};\n"

testCpp  =
  "#include <iostream>\n\
  \#include <vector>\n\
  \using namespace std;\n\
  \void test( vector<float>& vec ) {\n\
  \  cout << vec.size() << endl;\n\
  \}\n"


cabal = Cabal { cabal_pkgname = CabalName "testpkg"
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

vectorfloatref_ = TemplateAppRef t_vector "CFloat" "std::vector<float>"

classA =
  Class cabal "A" [ deletable ] mempty Nothing
    [ Constructor [ ] Nothing
    ]


classes = [ classA ]

toplevelfunctions =
  [ TopLevelFunction void_ "test" [ (vectorfloatref_, "vec") ] Nothing
  ]

templates = [  ]

headerMap =
  ModuleUnitMap $
    HM.fromList $
      [ ( MU_TopLevel
        , ModuleUnitImports {
            muimports_namespaces = [NS "std"]
          , muimports_headers = [HdrName "vector", HdrName "test.h"]
          }
        )
      , ( MU_Class "A"
        , ModuleUnitImports {
            muimports_namespaces = []
          , muimports_headers = [HdrName "test.h"]
          }
        )
      ]



main :: IO ()
main = do
  simpleBuilder
    "TestPkg"
    headerMap
    (cabal,classes,toplevelfunctions,templates)
    [ ]
    extraDep
