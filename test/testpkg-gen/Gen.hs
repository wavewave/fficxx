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
  [ Destructor Nothing ]
  []


-- import from stdcxx
string :: Class
string =
  Class stdcxx_cabal "string" [ ] mempty
  (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
  []
  []

t_vector = TmplCls stdcxx_cabal "Vector" "std::vector" (TParam_Simple "t") [ ]

testH =
  "#include <iostream>\n\
  \#include <vector>\n\
  \#include <string>\n\
  \using namespace std;\n\
  \void test( vector<float>&  vect);\n\
  \void test2( const string& x );\n\
  \class A {\n\
  \public:\n\
  \  A() {\n\
  \    cout << \"A created\" << endl;\n\
  \  }\n\
  \  virtual ~A() {\n\
  \    cout << \"A deleted\" << endl;\n\
  \  }\n\
  \  virtual void func( const string& s ) {\n\
  \  }\n\
  \  int member;\n\
  \  string member2;\n\
  \};\n\
  \class B {\n\
  \public:\n\
  \  B() {\n\
  \    cout << \"B created\" << endl;\n\
  \  }\n\
  \  virtual ~B() {\n\
  \    cout << \"B deleted\" << endl;\n\
  \  }\n\
  \  virtual void call( vector<float>& vect );\n\
  \  virtual vector<float> call2(); \n\
  \};\n"

testCpp  =
  "#include <iostream>\n\
  \#include <vector>\n\
  \#include \"test.h\"\n\
  \using namespace std;\n\
  \void test( vector<float>& vec ) {\n\
  \  cout << vec.size() << endl;\n\
  \}\n\
  \\n\
  \void test2( const string& s ) {\n\
  \  cout << s << endl;\n\
  \}\n\
  \\n\
  \void B::call( vector<float>& vec ) {\n\
  \  cout << \"in B::call\" << endl;\n\
  \  cout << vec.size() << endl;\n\
  \}\n\
  \\n\
  \vector<float> B::call2() {\n\
  \  vector<float> v;\n\
  \  v.push_back(1.0);\n\
  \  v.push_back(2.0);\n\
  \  v.push_back(3.0);\n\
  \  return v;\n\
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

vectorfloat_ = TemplateApp t_vector (TArg_Other "CFloat") "std::vector<float>"

vectorfloatref_ = TemplateAppRef t_vector (TArg_Other "CFloat") "std::vector<float>"

classA =
  Class cabal "A" [ deletable ] mempty Nothing
    [ Constructor [ ] Nothing
    , Virtual void_ "func" [ (cppclassref_ string, "s") ] Nothing
    ]
    [ Variable cint_ "member"
    , Variable (cppclasscopy_ string) "member2"
    ]

classB =
  Class cabal "B" [ deletable ] mempty Nothing
    [ Constructor [ ] Nothing
    , Virtual void_ "call" [ (vectorfloatref_, "vec") ] Nothing
    , Virtual vectorfloat_ "call2" [] Nothing
    ]
    [ ]

classes = [ classA, classB ]

toplevelfunctions =
  [ TopLevelFunction void_ "test" [ (vectorfloatref_, "vec") ] Nothing
  , TopLevelFunction void_ "test2" [ (cppclassref_ string, "x") ] Nothing
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
      , ( MU_Class "B"
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
