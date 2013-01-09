module Bindings.Cxx.Generate.Code.Dependency where

import Control.Applicative
import Data.List 
import Data.Maybe
import System.FilePath 
--
import Bindings.Cxx.Generate.Type.Class 
import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.CType

mkPkgHeaderFileName :: String  -- ^ package name 
                    -> Class 
                    -> String 
mkPkgHeaderFileName pkgname c = pkgname ++ (class_name c) ++ ".h"   

mkPkgCppFileName :: String  -- ^ package name 
                 -> Class 
                 -> String 
mkPkgCppFileName pkgname c = pkgname ++ (class_name c) ++ ".cpp"

mkPkgIncludeHeaders :: String   -- ^ package name 
                    -> Class 
                    -> [String] 
mkPkgIncludeHeaders pkgname c =
  let cs = class_allparents c
  in  map (mkPkgHeaderFileName pkgname) cs

{-
-- this function must be outsourced!
mkCROOTIncludeHeaders :: Class 
                      -> [String] 
mkCROOTIncludeHeaders c = 
  case class_name c of
    "Deletable" -> [] 
    _ -> [(class_name c) ++ ".h"]
-}

mkCIH :: (String,Class->[String])  -- ^ (package name, mkIncludeHeaders)  
      -> Class 
      -> ClassImportHeader
mkCIH (pkgname,mkincheaders) c = ClassImportHeader c 
                                   (mkPkgHeaderFileName pkgname c) 
                                   (mkPkgCppFileName pkgname c) 
                                   (mkPkgIncludeHeaders pkgname c) 
                                   (mkincheaders c)

-- |
extractClassFromType :: Types -> Maybe String
extractClassFromType Void = Nothing
extractClassFromType SelfType = Nothing
extractClassFromType (CT _ _) = Nothing
extractClassFromType (CPT (CPTClass str) _) = Just str

-- | 
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

-- | 
mkModuleDepRaw :: Class -> [String] 
mkModuleDepRaw c = 
  let fs = class_funcs c 
  in  nub . filter (/= class_name c) . concatMap (fst.extractClassDep) $ fs

-- | 
mkModuleDepHigh :: Class -> [String] 
mkModuleDepHigh c = 
  let fs = class_funcs c 
  in  nub . filter (/= class_name c)  $ 
        concatMap (snd.extractClassDep) fs
        ++ map class_name (class_parents c) 

-- | 
mkModuleDepFFI4One :: Class -> [String] 
mkModuleDepFFI4One c = 
  let fs = class_funcs c 
  in  (++) <$> concatMap (fst.extractClassDep)  <*> concatMap (snd.extractClassDep) $ fs

-- | 
mkModuleDepFFI :: Class -> [String] 
mkModuleDepFFI c = 
  let ps = class_allparents c 
      alldeps' = (concatMap mkModuleDepFFI4One ps) ++ mkModuleDepFFI4One c
      alldeps = nub (filter (/= class_name c) alldeps')
  in  alldeps

                    
mkClassModule :: (String,Class->[String])
              -> Class 
              -> ClassModule 
mkClassModule (pkgname,mkincheaders) c = 
    (ClassModule <$> getClassModuleBase  -- modulename -- class_name
                 <*> return
                 <*> return . mkCIH (pkgname,mkincheaders) 
                 <*> raws -- mkModuleDepRaw 
                 <*> highs -- mkModuleDepHigh 
                 <*> ffis -- mkModuleDepFFI 
    ) c
  where mbase = (cabal_moduleprefix.class_cabal) c
        raws = map ((<.>) mbase) . mkModuleDepRaw 
        highs = map ((<.>) mbase) . mkModuleDepHigh 
        ffis = map ((<.>) mbase) . mkModuleDepFFI 

-- where modulename = (<.>) <$> (cabal_moduleprefix.class_cabal) <*> class_name  

mkAllClassModulesAndCIH :: (String,Class->[String]) -- ^ (package name,mkIncludeHeaders)
                        -> [Class] 
                        -> ([ClassModule],[ClassImportHeader])
mkAllClassModulesAndCIH (pkgname,mkincheaders) cs = 
  let ms = map (mkClassModule (pkgname,mkincheaders)) cs 
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
  in (ms,cihs)

