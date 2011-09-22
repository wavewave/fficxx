module HROOT.Generate.ROOTModule where

exposedModules :: [String]
exposedModules = [ "HROOT" 
                 , "HROOT.Class.Interface"
                 , "HROOT.Class.Implementation"
                 , "HROOT.Class.FFI"
                 , "HROOT.AddOn" 
                 , "HROOT.AddOnFunction" ]

classModules :: [String] 
classModules = [ "TObject" 
               , "TNamed" ]
