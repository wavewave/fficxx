module Main where

import qualified Data.HashMap.Strict as HM
import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Cabal (Cabal(..),CabalName(..))
import FFICXX.Generate.Type.Config (ModuleUnitImports(..),ModuleUnitMap(..),ModuleUnit(..))
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = CabalName "stdcxx"
              , cabal_cheaderprefix = "STD"
              , cabal_moduleprefix = "STD"
              , cabal_additional_c_incs = []
              , cabal_additional_c_srcs = []
              , cabal_additional_pkgdeps = []
              , cabal_license = Just "BSD3"
              , cabal_licensefile = Just "LICENSE"
              , cabal_extraincludedirs = [ ]
              , cabal_extralibdirs = []
              , cabal_extrafiles = []
              , cabal_pkg_config_depends = []
              }

extraDep = [ ]


deletable :: Class
deletable =
  AbstractClass cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing ]
  []

string :: Class
string =
  Class cabal "string" [ deletable ] mempty
    (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
    [ Constructor [ cstring "p" ] Nothing
    , NonVirtual cstring_ "c_str" [] Nothing
    , NonVirtual (cppclassref_ string) "append" [cppclassref string "str"] Nothing
    , NonVirtual (cppclassref_ string) "erase" [] Nothing
    ]
    []


ostream :: Class
ostream = Class cabal "ostream" [] mempty
                (Just (ClassAlias { caHaskellName = "Ostream", caFFIName = "ostream" }))
                []
                []

classes = [ deletable
          --
          , ostream
          , string
          ]

toplevelfunctions = [ ]

t_vector = TmplCls cabal "Vector" "std::vector" (TParam_Simple "t")
             [ TFunNew [] Nothing
             , TFun void_ "push_back" "push_back" [(TemplateParam (TParam_Simple "t"),"x")] Nothing
             , TFun void_ "pop_back"  "pop_back"  []                        Nothing
             , TFun (TemplateParam (TParam_Simple "t")) "at" "at" [int "n"]                 Nothing
             , TFun int_  "size"      "size"      []                        Nothing
             , TFunDelete
             ]

t_unique_ptr = TmplCls cabal "UniquePtr" "std::unique_ptr" (TParam_Simple "t")
             [ TFunNew [] (Just "newUniquePtr0")
             , TFunNew [(TemplateParamPointer (TParam_Simple "t"), "p")] Nothing
             , TFun (TemplateParamPointer (TParam_Simple "t")) "get" "get" [] Nothing
             , TFun (TemplateParamPointer (TParam_Simple "t")) "release" "release" [] Nothing
             , TFun void_ "reset" "reset" [] Nothing
             , TFunDelete
             ]


t_shared_ptr = TmplCls cabal "SharedPtr" "std::shared_ptr" (TParam_Simple "t")
             [ TFunNew [] (Just "newSharedPtr0")
             , TFunNew [(TemplateParamPointer (TParam_Simple "t"), "p")] Nothing
             , TFun (TemplateParamPointer (TParam_Simple "t")) "get" "get" [] Nothing
             , TFun void_ "reset" "reset" [] Nothing
             , TFun int_ "use_count" "use_count" [] Nothing
             , TFunDelete
             ]

t_function = TmplCls cabal "Function" "std::function" (TParam_Function "t")
             [ TFunNew [(TemplateParam (TParam_Function "t"), "p")] Nothing
             , TFunDelete
             ]



templates = [ (t_vector    , HdrName "Vector.h")
            , (t_unique_ptr, HdrName "UniquePtr.h")
            , (t_shared_ptr, HdrName "SharedPtr.h")
            , (t_function  , HdrName "Function.h")
            ]



headerMap =
  ModuleUnitMap $
    HM.fromList [
      (MU_Class "string", ModuleUnitImports {
                            muimports_namespaces = [NS "std"]
                          , muimports_headers = [ HdrName "string" ]
                          })
    ]

main :: IO ()
main = do
  simpleBuilder
    "STD"
    headerMap
    (cabal,classes,toplevelfunctions,templates)
    [ ]
    extraDep
