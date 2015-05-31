import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.PackageInterface

snappyclasses = [ ] 

mycabal = Cabal { cabal_pkgname = "qt5" 
                , cabal_cheaderprefix = "Qt5"
                , cabal_moduleprefix = "Qt5" }

-- myclass = Class mycabal 

-- this is standard string library
--string :: Class 
--string = 
--  Class mycabal "string" [] mempty  (Just "CppString")
--  [ 
--  ]  


qApplication = 
  Class mycabal "QApplication" [] mempty Nothing
  [ Constructor [int "argc", charpp "argv"]  Nothing 
  , NonVirtual int_ "exec" [] Nothing 
  ]

qLabel = 
  Class mycabal "QLabel" [] mempty Nothing
  [ Constructor [ cstring "text" ] Nothing
  , NonVirtual void_ "show" [] Nothing ]

myclasses = [ qApplication, qLabel ] 

toplevelfunctions = []

headerMap = [ ("QApplication", ([], [HdrName "QtWidgets/qapplication.h"]))
            , ("QLabel"      , ([], [HdrName "QtWidgets/qlabel.h"]))
            ]

main :: IO ()
main = do 
  simpleBuilder "Qt5" headerMap (mycabal,myclasses,toplevelfunctions) ["Qt5Widgets", "Qt5Gui", "Qt5Core"]


