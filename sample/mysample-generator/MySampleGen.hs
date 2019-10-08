{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid
import System.Directory (getCurrentDirectory)
import FFICXX.Generate.Builder
import FFICXX.Generate.Config ( SimpleBuilderConfig(..) )
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface

incs = [ AddCInc "test.h" "//test ok?" ]

srcs = [ AddCSrc "test.cpp" "//test ok??" ]


mycabal = Cabal { cabal_pkgname = "MySample"
                , cabal_cheaderprefix = "MySample"
                , cabal_moduleprefix = "MySample"
                , cabal_additional_c_incs = incs
                , cabal_additional_c_srcs = srcs
                }

mycabalattr =
    CabalAttr
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = []
    , cabalattr_extralibdirs = []
    , cabalattr_extrafiles = []
    }


myclass = Class mycabal

a :: Class
a = myclass "A" [] mempty Nothing
    [ Constructor [] Nothing
    , Virtual void_ "method1" [ ] Nothing
    ]

b :: Class
b = myclass "B" [a] mempty Nothing
    [ Constructor [] Nothing
    , Virtual (cppclass_ a) "method2" [ cppclass a "x" ] Nothing
    ]

myclasses = [ a, b]

toplevelfunctions = [ ]


main :: IO ()
main =
  cwd <- getCurrentDirectory
  let cfg = FFICXXConfig {
              fficxxconfig_scriptBaseDir = cwd
            , fficxxconfig_workingDir = cwd </> "working"
            , fficxxconfig_installBaseDir = dir </> unCabalName pkgname
            }
      sbc = SimpleBuilderConfig {
              sbcTopModule  = "MySample"
            , sbcModUnitMap = mempty
            , sbcCabal      = mycabal
            , sbcClasses    = myclasses
            , sbcTopLevels  = toplevelfunctions
            , sbcTemplates  = []
            , sbcExtraLibs  = []
            , sbcExtraDeps  = []
            }
  simpleBuilder cfg sbc
