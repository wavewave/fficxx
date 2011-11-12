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

extractClassDep :: Function -> ([String],[String])
extractClassDep (Constructor args)  = 
    ([],catMaybes (map (extractClassFromType.fst) args))
extractClassDep (Virtual ret _ args) = 
    (catMaybes [extractClassFromType ret], mapMaybe (extractClassFromType.fst) args)
extractClassDep (NonVirtual ret _ args) = 
    (catMaybes [extractClassFromType ret], mapMaybe (extractClassFromType.fst) args)
extractClassDep (Static ret _ args) = 
    (catMaybes [extractClassFromType ret], mapMaybe (extractClassFromType.fst) args)
extractClassDep (AliasVirtual ret _ args _) = 
    (catMaybes [extractClassFromType ret], mapMaybe (extractClassFromType.fst) args)
extractClassDep Destructor = ([],[]) 

mkModuleDepRaw :: Class -> [String] 
mkModuleDepRaw c = 
  let fs = class_funcs c 
  in  nub . filter (/= class_name c) . concatMap (fst.extractClassDep) $ fs

mkModuleDepHigh :: Class -> [String] 
mkModuleDepHigh c = 
  let fs = class_funcs c 
  in  nub . filter (/= class_name c)  $ 
        concatMap (snd.extractClassDep) fs
        ++ map class_name (class_parents c) 


mkModuleDepFFI4One :: Class -> [String] 
mkModuleDepFFI4One c = 
  let fs = class_funcs c 
  in  (++) <$> concatMap (fst.extractClassDep)  <*> concatMap (snd.extractClassDep) $ fs
--      ++ concatMap (snd.ex
mkModuleDepFFI :: Class -> [String] 
mkModuleDepFFI c = 
  let ps = class_allparents c 
      alldeps' = (concatMap mkModuleDepFFI4One ps) ++ mkModuleDepFFI4One c
      alldeps = nub (filter (/= class_name c) alldeps')
  in  alldeps

{-
mkModuleDepHigh4One :: Class -> [String] 
mkModuleDepHigh4One c = 
  let fs = class_funcs c 
  in  concatMap (snd.extractClassDep) fs

mkModuleDepHigh :: Class -> [String]
mkModuleDepHigh c = 
  let ps = class_allparents c 
      alldeps' = (concatMap mkModuleDepHigh4One ps) ++ mkModuleDepHigh4One c
                 ++ map class_name (class_allparents c) 
      alldeps = nub (filter (/= class_name c) alldeps')
  in  alldeps
-}
                    
mkClassModule :: Class -> ClassModule 
mkClassModule = 
  ClassModule <$> class_name
              <*> return
              <*> return.mkCIH
              <*> mkModuleDepRaw
              <*> mkModuleDepHigh
              <*> mkModuleDepFFI 

mkAllClassModulesAndCIH :: [Class] -> ([ClassModule],[ClassImportHeader])
mkAllClassModulesAndCIH cs = 
  let ms = map mkClassModule cs 
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
  in (ms,cihs)

