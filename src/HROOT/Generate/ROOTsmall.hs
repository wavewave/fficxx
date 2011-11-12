-- |
-- Module      : HROOT.Generate.ROOTsmall
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Generate.ROOTsmall where

import HROOT.Generate.Type.CType
import HROOT.Generate.Type.Method
import HROOT.Generate.Type.Class
import HROOT.Generate.Type.Module


moduleInterface :: Module
moduleInterface = Module { module_name = "HROOT.Class.Interface"
                         , module_exports = [ "IDeletable" 
                                            , "TObject"
                                            , "ITObject"
                                            , "ITNamed" 
                                            , "TNamed" ]
                         }  

deletable :: Class 
deletable = AbstractClass "Deletable" [] 
          [ Destructor ]

deletableH = ClassImportHeader deletable "HROOTDeletable.h" "HROOTDeletable.cpp"
                               [] [] 
        
deletableM = ClassModule "Deletable" [deletable] [deletableH] [] [] []



tObject :: Class
tObject = 
  Class "TObject" [deletable] 
  [ Constructor [] 
  -- , Virtual int_ "DistancetoPrimitive" [int "px", int "py"]
  , Virtual void_    "Draw"    [cstring "option"] 
  -- , Virtual void_ "ExecuteEvent" [int "event", int "px", int "py"]
  , Virtual (cppclass_ "TObject") "FindObject" [cstring "name"]
  , Virtual  cstring_ "GetName" [] 
  , Virtual (cppclass_ "TClass") "IsA" [] 
  , Virtual void_ "Paint" [cstring "option"] 
  , AliasVirtual void_ "Print" [cstring "option"] "printObj"
  , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] 
  , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ]

  , Static  bool_    "GetObjectStat" []
  ]

tObjectH = ClassImportHeader tObject "HROOTTObject.h" "HROOTTObject.cpp" 
                             [ "HROOTDeletable.h" ]
                             [ "TObject.h" ]
                             
tObjectM = ClassModule "TObject" [tObject] [tObjectH] ["TClass"] ["Deletable"] []

tDictionary :: Class
tDictionary = AbstractClass "TDictionary" [tNamed]
              [
              ]

tDictionaryH = ClassImportHeader tDictionary "HROOTTDictionary.h" "HROOTTDictionary.cpp" 
                                 [ "HROOTDeletable.h"
                                 , "HROOTTObject.h"
                                 , "HROOTTNamed.h"
                                 ] 
                                 [ "TDictionary.h" ]

tDictionaryM = ClassModule "TDictionary" [tDictionary] [tDictionaryH] [] ["Deletable","TObject","TNamed"] []

tNamed :: Class
tNamed = 
  Class "TNamed" [tObject] 
  [ Constructor [cstring "name", cstring "title"] 
  , Virtual void_  "SetName"      [cstring "name"]
  , Virtual void_  "SetNameTitle" [cstring "name", cstring "title"]
  , Virtual void_  "SetTitle"     [cstring "name"]  
  ]

tNamedH = ClassImportHeader tNamed "HROOTTNamed.h" "HROOTTNamed.cpp"  
                            [ "HROOTDeletable.h"
                            , "HROOTTObject.h"
                            ]
                            [ "TNamed.h" ]
                           
tNamedM = ClassModule "TNamed"  [tNamed] [tNamedH] ["TObject","TClass"] ["Deletable","TObject"] []

tClass :: Class
tClass = Class "TClass" [tDictionary]
         [
         ]

tClassH = ClassImportHeader tClass "HROOTTClass.h" "HROOTTClass.cpp"  
                            [ "HROOTDeletable.h"
                            , "HROOTTObject.h"
                            , "HROOTTNamed.h"
                            , "HROOTTDictionary.h"
                            ]
                            [ "TClass.h" ]

tClassM = ClassModule "TClass" [tClass] [tClassH] ["TObject"] ["Deletable","TObject","TNamed","TDictionary"] []


root_all_classes :: [Class]
root_all_classes = 
  [ deletable, tObject, tClass, tDictionary, tNamed ] 

{-
root_all_classes_imports :: [ClassImportHeader]
root_all_classes_imports = 
  [ deletableH
  , tObjectH
  , tClassH 
  , tDictionaryH 
  , tNamedH ] 

root_all_modules :: [ClassModule]
root_all_modules = 
  [ deletableM
  , tObjectM
  , tClassM
  , tDictionaryM
  , tNamedM ] 
-}
