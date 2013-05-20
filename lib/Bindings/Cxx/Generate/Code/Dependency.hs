module Bindings.Cxx.Generate.Code.Dependency where

import Control.Applicative
import Data.Function (on)
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
mkPkgIncludeHeadersInH :: Class -> [String] 
mkPkgIncludeHeadersInH c =
    let pkgname = (cabal_pkgname . class_cabal) c
        extclasses = (filter ((/= pkgname) . cabal_pkgname . class_cabal) . mkModuleDepCpp) c
        extheaders = nub . map ((++"Type.h") .  cabal_pkgname . class_cabal) $ extclasses  
    in map mkPkgHeaderFileName (class_allparents c) ++ extheaders

    -- (map mkPkgHeaderFileName . class_allparents) c 
                           

-- | 
mkPkgIncludeHeadersInCPP :: Class -> [String] 
mkPkgIncludeHeadersInCPP = map mkPkgHeaderFileName . mkModuleDepCpp 


-- mkModuleDepFFI4One

-- mkModuleDepHigh -- class_allparents 


mkCIH :: (Class->([Namespace],[String]))  -- ^ (mk namespace and include headers)  
      -> Class 
      -> ClassImportHeader
mkCIH mkNSandIncHdrs c = let r = ClassImportHeader c 
                                   (mkPkgHeaderFileName c) 
                                   ((fst . mkNSandIncHdrs) c)
                                   (mkPkgCppFileName c) 
                                   (mkPkgIncludeHeadersInH c) 
                                   (mkPkgIncludeHeadersInCPP c)
                                   ((snd . mkNSandIncHdrs) c)
                         in r -- trace (show r) r 


-- |
extractClassFromType :: Types -> Maybe Class
extractClassFromType Void = Nothing
extractClassFromType SelfType = Nothing
extractClassFromType (CT _ _) = Nothing
extractClassFromType (CPT (CPTClass c) _) = Just c
extractClassFromType (CPT (CPTClassRef c) _) = Just c


-- | class dependency for a given function 
data Dep4Func = Dep4Func { returnDependency :: Maybe Class 
                         , argumentDependency :: [Class] }

{- 
-- | 
data Dep4Interface = Dep4Interface 
                     { parentDependency :: [Class] 
                     , interfaceDependency :: [Class] 
                     , typeDependecny :: [Class] 
                     } 
-}

-- | 
extractClassDep :: Function -> Dep4Func {- ([Class],[Class])  -- ^ (rettypedep,argtypedep)  -} 
extractClassDep (Constructor args)  = Dep4Func Nothing (catMaybes (map (extractClassFromType.fst) args))
extractClassDep (Virtual ret _ args) = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep (NonVirtual ret _ args) =
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep (Static ret _ args) = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep (AliasVirtual ret _ args _) = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep Destructor = 
    Dep4Func Nothing [] 

-- | 
mkModuleDepRaw :: Class -> [Class] 
mkModuleDepRaw c = (nub . filter (/= c) . mapMaybe (returnDependency.extractClassDep) . class_funcs) c
        -- ++ concatMap (snd.extractClassDep) fs        

-- | 
mkModuleDepHighNonSource :: Class -> [Class] 
mkModuleDepHighNonSource c = 
  let fs = class_funcs c 
      pkgname = (cabal_pkgname . class_cabal) c 
      extclasses = (filter (\x-> x /= c && ((/= pkgname) . cabal_pkgname . class_cabal) x) . concatMap (argumentDependency.extractClassDep)) fs
      parents = class_parents c 
  in  nub (parents ++ extclasses) 


-- | 
mkModuleDepHighSource :: Class -> [Class] 
mkModuleDepHighSource c = 
  let fs = class_funcs c 
      pkgname = (cabal_pkgname . class_cabal) c 
  in  nub . filter (\x-> x /= c && not (x `elem` class_parents c) && (((== pkgname) . cabal_pkgname . class_cabal) x)) . concatMap (argumentDependency.extractClassDep) $ fs

-- | 
mkModuleDepCpp :: Class -> [Class] 
mkModuleDepCpp c = 
  let fs = class_funcs c 
  in  nub . filter (/= c)  $ 
        mapMaybe (returnDependency.extractClassDep) fs   
        ++ concatMap (argumentDependency.extractClassDep) fs
        ++ (class_parents c) 

-- | 
mkModuleDepFFI4One :: Class -> [Class] 
mkModuleDepFFI4One c = 
  let fs = class_funcs c 
  in  (++) <$> mapMaybe (returnDependency.extractClassDep)  
           <*> concatMap (argumentDependency.extractClassDep) 
      $ fs

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
    let r = (ClassModule <$> getClassModuleBase  
                 <*> pure
                 <*> return . mkCIH mkincheaders
                 <*> highs_nonsource  
                 <*> raws 
                 <*> highs_source
                 <*> ffis 
            ) c
    in r -- trace (show r) r 
    
  where highs_nonsource = map getClassModuleBase . mkModuleDepHighNonSource
        raws = map getClassModuleBase . mkModuleDepRaw 
        highs_source = map getClassModuleBase . mkModuleDepHighSource
        ffis = map getClassModuleBase . mkModuleDepFFI 

 
mkAllClassModulesAndCIH :: (String,Class->([Namespace],[String])) -- ^ (package name,mkIncludeHeaders)
                        -> [Class] 
                        -> ([ClassModule],[ClassImportHeader])
mkAllClassModulesAndCIH (pkgname,mkNSandIncHdrs) cs = 
  let ms = map (mkClassModule (pkgname,mkNSandIncHdrs)) cs 
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
  in (ms,cihs)

mkHSBOOTCandidateList :: [ClassModule] -> [String]
mkHSBOOTCandidateList ms = nub (concatMap cmImportedModulesHighSource ms)
