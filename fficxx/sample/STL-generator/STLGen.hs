import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = "STL" 
              , cabal_cheaderprefix = "STL"
              , cabal_moduleprefix = "STL" }

-- myclass = Class mycabal 

-- this is standard string library

classes = [ ] 

toplevelfunctions =  [ ]  



templates = [ TmplCls cabal "Vector"
                [ TFun void_ "func1" [] Nothing
                ]
            ] 


headerMap = [ ]

cabalattr = 
    CabalAttr 
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = []
    , cabalattr_extralibdirs = []
    }

main :: IO ()
main = do 
  simpleBuilder "STL" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates) [ "stl" ]


