module HROOT.Generate.Code.Dependency where

import Control.Applicative
import Data.List 
import Data.Maybe
import HROOT.Generate.Type.Class 
import HROOT.Generate.Type.Method
import HROOT.Generate.Type.CType

mkHROOTHeaderFileName :: Class -> String 
mkHROOTHeaderFileName c = "HROOT" ++ (class_name c) ++ ".h"   

mkHROOTCppFileName :: Class -> String 
mkHROOTCppFileName c = "HROOT" ++ (class_name c) ++ ".cpp"

mkHROOTIncludeHeaders :: Class -> [String] 
mkHROOTIncludeHeaders c =
  let cs = class_allparents c
  in  map mkHROOTHeaderFileName cs

mkCROOTIncludeHeaders :: Class -> [String] 
mkCROOTIncludeHeaders c = 
  case class_name c of
    "Deletable" -> [] 
    _ -> [(class_name c) ++ ".h"]

mkCIH :: Class -> ClassImportHeader
mkCIH c = ClassImportHeader c (mkHROOTHeaderFileName c) (mkHROOTCppFileName c) (mkHROOTIncludeHeaders c) (mkCROOTIncludeHeaders c)

extractClassFromType :: Types -> Maybe String
extractClassFromType Void = Nothing
extractClassFromType SelfType = Nothing
extractClassFromType (CT _ _) = Nothing
extractClassFromType (CPT (CPTClass str) _) = Just str

extractClassDep :: Function -> [String] 
extractClassDep (Constructor args)  = 
    catMaybes (map (extractClassFromType.fst) args)
extractClassDep (Virtual ret _ args) = 
    catMaybes ((extractClassFromType ret) : map (extractClassFromType.fst) args)
extractClassDep (NonVirtual ret _ args) = 
    catMaybes ((extractClassFromType ret) : map (extractClassFromType.fst) args)
extractClassDep (AliasVirtual ret _ args _) = 
    catMaybes ((extractClassFromType ret) : map (extractClassFromType.fst) args)
extractClassDep Destructor = [] 

mkModuleDepRaw4One :: Class -> [String] 
mkModuleDepRaw4One c = 
  let fs = class_funcs c 
  in  concatMap extractClassDep fs

mkModuleDepRaw :: Class -> [String] 
mkModuleDepRaw c = 
  let ps = class_allparents c 
      alldeps' = (concatMap mkModuleDepRaw4One ps) ++ mkModuleDepRaw4One c
      alldeps = nub (filter (/= class_name c) alldeps')
  in  alldeps

mkModuleDepHigh :: Class -> [String]
mkModuleDepHigh c = map class_name (class_allparents c)

mkClassModule :: Class -> ClassModule 
mkClassModule = 
  ClassModule<$>class_name<*>return<*>return.mkCIH<*>mkModuleDepRaw<*>mkModuleDepHigh 

mkAllClassModulesAndCIH :: [Class] -> ([ClassModule],[ClassImportHeader])
mkAllClassModulesAndCIH cs = 
  let ms = map mkClassModule cs 
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
  in (ms,cihs)

