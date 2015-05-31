import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.PackageInterface

snappyclasses = [ ] 

mycabal = Cabal { cabal_pkgname = "qt5" 
                , cabal_cheaderprefix = "Qt5"
                , cabal_moduleprefix = "Qt5" }



qObject = 
  Class mycabal "QObject" [] mempty Nothing
  [ ] 

qCoreApplication = 
  Class mycabal "QCoreApplication" [qObject] mempty Nothing
  [ ]

qGuiApplication =
  Class mycabal "QGuiApplication" [qCoreApplication] mempty Nothing
  [ ]

qApplication = 
  Class mycabal "QApplication" [qGuiApplication] mempty Nothing
  [ Constructor [intref "argc", charpp "argv"]  Nothing 
  , Virtual int_ "exec" [] Nothing 
  ]


qWidget = 
  Class mycabal "QWidget" [qObject] mempty Nothing
  [ Virtual void_ "show" [] (Just "show1")
  ]


qFrame = 
  Class mycabal "QFrame" [qWidget] mempty Nothing
  [ ]

qLabel = 
  Class mycabal "QLabel" [qFrame] mempty Nothing
  [ Constructor [ cstring "text" ] Nothing
  ]

myclasses = [ qObject, qCoreApplication, qGuiApplication, qApplication
            , qWidget, qFrame
            , qLabel ] 

toplevelfunctions = []

headerMap = 
  [ ("QObject"         , ([], [HdrName "QtCore/qobject.h"]))
  , ("QCoreApplication", ([], [HdrName "QtCore/qcoreapplication.h"]))
  , ("QGuiApplication" , ([], [HdrName "QtGui/qguiapplication.h"]))
  , ("QApplication"    , ([], [HdrName "QtWidgets/qapplication.h"]))
  , ("QWidget"         , ([], [HdrName "QtWidgets/qwidget.h"]))
  , ("QFrame"          , ([], [HdrName "QtWidgets/qframe.h"]))
  , ("QLabel"          , ([], [HdrName "QtWidgets/qlabel.h"]))
  ]

mycabalattr = 
    CabalAttr 
    { cabalattr_license = Nothing
    , cabalattr_licensefile = Nothing
    , cabalattr_extraincludedirs = ["/home/wavewave/.nix-profile/include"]
    , cabalattr_extralibdirs = ["/home/wavewave/.nix-profile/lib"  ]
    }

main :: IO ()
main = do 
  simpleBuilder "Qt5" headerMap (mycabal,mycabalattr,myclasses,toplevelfunctions) ["Qt5Widgets", "Qt5Gui", "Qt5Core"]


