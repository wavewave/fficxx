{-# LANGUAGE OverloadedStrings #-}

module Gen where

import qualified Data.HashMap.Strict as HM
import Data.Monoid (mempty)
--

--
import FFICXX.Generate.Builder (simpleBuilder)
import FFICXX.Generate.Code.Primitive
  ( cppclassref,
    cppclassref_,
    cstring,
    cstring_,
    int,
    int_,
    void_,
  )
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Type.Cabal
  ( BuildType (Simple),
    Cabal (..),
    CabalName (..),
  )
import FFICXX.Generate.Type.Class
  ( Arg (..),
    Class (..),
    ClassAlias (..),
    Form (FormNested, FormSimple),
    Function (..),
    OpExp (..),
    TemplateAppInfo (..),
    TemplateArgType (..),
    TemplateClass (..),
    TemplateFunction (..),
    TopLevel,
    Types (..),
    Variable (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
  )
import FFICXX.Generate.Type.Module (TemplateClassImportHeader (..))
import FFICXX.Generate.Type.PackageInterface ()
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

cabal :: Cabal
cabal =
  Cabal
    { cabal_pkgname = CabalName "stdcxx",
      cabal_version = "0.6.999",
      cabal_cheaderprefix = "STD",
      cabal_moduleprefix = "STD",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [],
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

deletable :: Class
deletable =
  AbstractClass
    cabal
    "Deletable"
    []
    mempty
    Nothing
    [Destructor Nothing]
    []
    []

string :: Class
string =
  Class
    cabal
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

ostream :: Class
ostream =
  Class
    cabal
    "ostream"
    []
    mempty
    (Just (ClassAlias {caHaskellName = "Ostream", caFFIName = "ostream"}))
    []
    []
    []
    False

classes :: [Class]
classes =
  [ deletable,
    --
    ostream,
    string
  ]

toplevels :: [TopLevel]
toplevels = []

t_pair :: TemplateClass
t_pair =
  TmplCls
    cabal
    "Pair"
    (FormSimple "std::pair")
    ["tp1", "tp2"]
    [ TFunNew [Arg (TemplateParam "tp1") "x", Arg (TemplateParam "tp2") "y"] Nothing,
      TFunDelete
    ]
    [ Variable $ Arg (TemplateParam "tp1") "first",
      Variable $ Arg (TemplateParam "tp2") "second"
    ]

t_map :: TemplateClass
t_map =
  TmplCls
    cabal
    "Map"
    (FormSimple "std::map")
    ["tpk", "tpv"]
    [ TFunNew [] Nothing,
      TFun
        ( TemplateAppRef
            TemplateAppInfo
              { tapp_tclass = t_map_iterator,
                tapp_tparams = [TArg_TypeParam "tpk", TArg_TypeParam "tpv"],
                tapp_CppTypeForParam = "std::map<tpk,tpv>::iterator"
              }
        )
        "begin"
        "begin"
        [],
      TFun
        ( TemplateAppRef
            TemplateAppInfo
              { tapp_tclass = t_map_iterator,
                tapp_tparams = [TArg_TypeParam "tpk", TArg_TypeParam "tpv"],
                tapp_CppTypeForParam = "std::map<tpk,tpv>::iterator"
              }
        )
        "end"
        "end"
        [],
      TFun
        void_ -- until pair<iterator,bool> is allowed
        "insert"
        "insert"
        [ Arg
            ( TemplateAppMove
                TemplateAppInfo
                  { tapp_tclass = t_pair,
                    tapp_tparams = [TArg_TypeParam "tpk", TArg_TypeParam "tpv"],
                    tapp_CppTypeForParam = "std::pair<tpk,tpv>"
                  }
            )
            "val"
        ],
      TFun int_ "size" "size" [],
      TFunDelete
    ]
    []

t_map_iterator :: TemplateClass
t_map_iterator =
  TmplCls
    cabal
    "MapIterator"
    (FormNested "std::map" "iterator")
    ["tpk", "tpv"]
    [ TFunOp
        { tfun_ret =
            TemplateApp
              TemplateAppInfo
                { tapp_tclass = t_pair,
                  tapp_tparams = [TArg_TypeParam "tpk", TArg_TypeParam "tpv"],
                  tapp_CppTypeForParam = "std::pair<tpk,tpv>"
                },
          tfun_name = "deRef",
          tfun_opexp = OpStar
        },
      TFunOp
        { tfun_ret -- TODO: this should be handled with self
          =
            TemplateApp
              TemplateAppInfo
                { tapp_tclass = t_map_iterator,
                  tapp_tparams = [TArg_TypeParam "tpk", TArg_TypeParam "tpv"],
                  tapp_CppTypeForParam = "std::map<tpk,tpv>::iterator"
                },
          tfun_name = "increment",
          tfun_opexp = OpFPPlus
        }
    ]
    []

t_vector :: TemplateClass
t_vector =
  TmplCls
    cabal
    "Vector"
    (FormSimple "std::vector")
    ["tp1"]
    [ TFunNew [] Nothing,
      TFun
        ( TemplateAppRef
            TemplateAppInfo
              { tapp_tclass = t_vector_iterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "std::vector<tp1>::iterator"
              }
        )
        "begin"
        "begin"
        [],
      TFun
        ( TemplateAppRef
            TemplateAppInfo
              { tapp_tclass = t_vector_iterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "std::vector<tp1>::iterator"
              }
        )
        "end"
        "end"
        [],
      TFun void_ "push_back" "push_back" [Arg (TemplateParam "tp1") "x"],
      TFun void_ "pop_back" "pop_back" [],
      TFun (TemplateParam "tp1") "at" "at" [int "n"],
      TFun int_ "size" "size" [],
      TFunDelete
    ]
    []

t_vector_iterator :: TemplateClass
t_vector_iterator =
  TmplCls
    cabal
    "VectorIterator"
    (FormNested "std::vector" "iterator")
    ["tp1"]
    [ TFunOp
        { tfun_ret = TemplateParam "tp1",
          tfun_name = "deRef",
          tfun_opexp = OpStar
        },
      TFunOp
        { tfun_ret -- TODO: this should be handled with self
          =
            TemplateApp
              TemplateAppInfo
                { tapp_tclass = t_vector_iterator,
                  tapp_tparams = [TArg_TypeParam "tp1"],
                  tapp_CppTypeForParam = "std::vector<tp1>::iterator"
                },
          tfun_name = "increment",
          tfun_opexp = OpFPPlus
        }
    ]
    []

t_unique_ptr :: TemplateClass
t_unique_ptr =
  TmplCls
    cabal
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

t_shared_ptr :: TemplateClass
t_shared_ptr =
  TmplCls
    cabal
    "SharedPtr"
    (FormSimple "std::shared_ptr")
    ["tp1"]
    [ TFunNew [] (Just "newSharedPtr0"),
      TFunNew [Arg (TemplateParamPointer "tp1") "p"] Nothing,
      TFun (TemplateParamPointer "tp1") "get" "get" [],
      TFun void_ "reset" "reset" [],
      TFun int_ "use_count" "use_count" [],
      TFunDelete
    ]
    []

templates :: [TemplateClassImportHeader]
templates =
  [ TCIH t_pair ["utility"],
    TCIH t_map ["map"],
    TCIH t_map_iterator ["map"],
    TCIH t_vector ["vector"],
    TCIH t_vector_iterator ["vector"],
    TCIH t_unique_ptr ["memory"],
    TCIH t_shared_ptr ["memory"]
  ]

headers :: [(ModuleUnit, ModuleUnitImports)]
headers =
  [modImports "string" ["std"] ["string"]]

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let cfg =
        FFICXXConfig
          { fficxxconfig_workingDir = cwd </> "working",
            fficxxconfig_installBaseDir = cwd </> "stdcxx",
            fficxxconfig_staticFileDir = ""
          }
      sbc =
        SimpleBuilderConfig
          { sbcTopModule = "STD",
            sbcModUnitMap = ModuleUnitMap (HM.fromList headers),
            sbcCabal = cabal,
            sbcClasses = classes,
            sbcTopLevels = toplevels,
            sbcTemplates = templates,
            sbcExtraLibs = ["stdc++"],
            sbcExtraDeps = extraDep,
            sbcStaticFiles = []
          }
  simpleBuilder cfg sbc
