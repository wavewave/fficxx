module Bindings.Cxx.Generate.Code.Dependency where

import Control.Applicative
import Data.List 
import Data.Maybe
import System.FilePath 
--
import Bindings.Cxx.Generate.Type.Class 
-- 
import Debug.Trace 

-- | 
mkPkgHeaderFileName ::Class -> String 
mkPkgHeaderFileName c = 
    (cabal_cheaderprefix.class_cabal) c ++ class_name c <.> "h" 
    -- pkgname ++ (class_name c) ++ ".h"   

-- | 
mkPkgCppFileName ::Class -> String 
mkPkgCppFileName c = 
    (cabal_cheaderprefix.class_cabal) c ++ class_name c <.> "cpp"
    -- pkgname ++ (class_name c) ++ ".cpp"

-- | 
mkPkgIncludeHeaders :: Class -> [String] 
mkPkgIncludeHeaders = map mkPkgHeaderFileName . mkModuleDepHigh -- class_allparents 


mkCIH :: (Class->([Namespace],[String]))  -- ^ (mk namespace and include headers)  
      -> Class 
      -> ClassImportHeader
mkCIH mkNSandIncHdrs c = let r = ClassImportHeader c 
                                   (mkPkgHeaderFileName c) 
                                   ((fst . mkNSandIncHdrs) c)
                                   (mkPkgCppFileName c) 
                                   (mkPkgIncludeHeaders c) 
                                   ((snd . mkNSandIncHdrs) c)
                         in trace (show r) $ r 


-- |
extractClassFromType :: Types -> Maybe Class
extractClassFromType Void = Nothing
extractClassFromType SelfType = Nothing
extractClassFromType (CT _ _) = Nothing
extractClassFromType (CPT (CPTClass c) _) = Just c
extractClassFromType (CPT (CPTClassRef c) _) = Just c

-- | 
extractClassDep :: Function -> ([Class],[Class])
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
mkModuleDepRaw :: Class -> [Class] 
mkModuleDepRaw c = 
  let fs = class_funcs c 
  in  nub . filter (/= c) . concatMap (fst.extractClassDep) $ fs

-- | 
mkModuleDepHigh :: Class -> [Class] 
mkModuleDepHigh c = 
  let fs = class_funcs c 
  in  nub . filter (/= c)  $ 
        concatMap (snd.extractClassDep) fs
        ++ (class_parents c) 

-- | 
mkModuleDepFFI4One :: Class -> [Class] 
mkModuleDepFFI4One c = 
  let fs = class_funcs c 
  in  (++) <$> concatMap (fst.extractClassDep)  <*> concatMap (snd.extractClassDep) $ fs

-- | 
mkModuleDepFFI :: Class -> [Class] 
mkModuleDepFFI c = 
  let ps = class_allparents c 
      alldeps' = (concatMap mkModuleDepFFI4One ps) ++ mkModuleDepFFI4One c
      alldeps = nub (filter (/= c) alldeps')
  in  alldeps

                    
mkClassModule :: (String,Class->([Namespace],[String]))
              -> Class 
              -> ClassModule 
mkClassModule (pkgname,mkincheaders) c = 
    (ClassModule <$> getClassModuleBase  
                 <*> return
                 <*> return . mkCIH mkincheaders
                 <*> raws 
                 <*> highs 
                 <*> ffis 
    ) c
  where raws = map getClassModuleBase . mkModuleDepRaw 
        highs = map getClassModuleBase . mkModuleDepHigh 
        ffis = map getClassModuleBase . mkModuleDepFFI 


mkAllClassModulesAndCIH :: (String,Class->([Namespace],[String])) -- ^ (package name,mkIncludeHeaders)
                        -> [Class] 
                        -> ([ClassModule],[ClassImportHeader])
mkAllClassModulesAndCIH (pkgname,mkNSandIncHdrs) cs = 
  let ms = map (mkClassModule (pkgname,mkNSandIncHdrs)) cs 
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
  in (ms,cihs)

