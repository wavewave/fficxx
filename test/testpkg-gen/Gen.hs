module Main where

import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Cabal (Cabal(..),CabalName(..))
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

--
-- Tensorflow Serving 
-- 

cabal = Cabal { cabal_pkgname = CabalName "testpkg"
              , cabal_cheaderprefix = "TestPkg"
              , cabal_moduleprefix = "TestPkg"
              , cabal_additional_c_incs = []
              , cabal_additional_c_srcs = []
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

classes = [ ] 

toplevelfunctions =
  [ TopLevelFunction void_ "test" [ (vectorfloatref_, "vec") ] Nothing
  ]

templates = [  ]

headerMap =
  [
  ]

main :: IO ()
main = do
  simpleBuilder
    "TestPkg"
    headerMap
    (cabal,classes,toplevelfunctions,templates)
    [ ]
    extraDep
