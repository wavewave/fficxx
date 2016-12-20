import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = "STL" 
              , cabal_cheaderprefix = "STL"
              , cabal_moduleprefix = "STL" }

classes = [ ] 

toplevelfunctions =  [ ]  



templates = [ ( TmplCls cabal "Vector" "std::vector" "t"
                  [ TFunNew []
                  , TFun void_ "push_back" "push_back" [(TemplateParam "t","x")] Nothing
                  , TFun void_ "pop_back"  "pop_back"  []                        Nothing
                  , TFun (TemplateParam "t") "at" "at" [int "n"]                 Nothing
                  , TFun int_  "size"      "size"      []                        Nothing
                  , TFunDelete
                  ]
              , HdrName "Vector.h" ) 
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
  simpleBuilder "STL" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates) [ ]


