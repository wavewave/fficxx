-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Dependency
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.Dependency where

import Control.Applicative
import Data.Function (on)
import Data.List 
import Data.Maybe
import System.FilePath 
--
import FFICXX.Generate.Type.Class 
import FFICXX.Generate.Type.Internal
--
-- import Debug.Trace 

-- | 
mkPkgHeaderFileName ::Class -> String 
mkPkgHeaderFileName c = 
    (cabal_cheaderprefix.class_cabal) c ++ class_name c <.> "h" 

-- | 
mkPkgCppFileName ::Class -> String 
mkPkgCppFileName c = 
    (cabal_cheaderprefix.class_cabal) c ++ class_name c <.> "cpp"

-- | 
mkPkgIncludeHeadersInH :: Class -> [String] 
mkPkgIncludeHeadersInH c =
    let pkgname = (cabal_pkgname . class_cabal) c
        extclasses = (filter ((/= pkgname) . cabal_pkgname . class_cabal) . mkModuleDepCpp) c
        extheaders = nub . map ((++"Type.h") .  cabal_pkgname . class_cabal) $ extclasses  
    in map mkPkgHeaderFileName (class_allparents c) ++ extheaders

                           

-- | 
mkPkgIncludeHeadersInCPP :: Class -> [String] 
mkPkgIncludeHeadersInCPP = map mkPkgHeaderFileName . mkModuleDepCpp 


-- | 
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
                         in r 


-- | Get a class definition from a CPPType
extractClassFromType ::  CPPType a -> Maybe a
extractClassFromType (PrimType (CPTClass c)) = Just c
extractClassFromType _ = Nothing

-- |
{-extractClassFromType :: Types -> Maybe Class-}
{-extractClassFromType Void = Nothing-}
{-extractClassFromType SelfType = Nothing-}
{-extractClassFromType (CT _ _) = Nothing-}
{-extractClassFromType (CPT (CPTClass c) _) = Just c-}
{-extractClassFromType (CPT (CPTClassRef c) _) = Just c-}


-- | class dependency for a given function 
data Dep4Func = Dep4Func { returnDependency :: Maybe Class 
                         , argumentDependency :: [Class] }

-- | 
-- FIXME: 'extractClassDep' is a confusing name, should it be extractFuncDep?
extractClassDep :: MethodMemberType Class -> Dep4Func 
extractClassDep (Constructor _ args _)  =
  Dep4Func Nothing (catMaybes (map (extractClassFromType.fst) args))
extractClassDep (Destructor _ _ _ _) = Dep4Func Nothing [] 
extractClassDep (NormalMethod _ _ args ret _) =
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)

extractClassDepForTopLevelFunction :: TopLevelFunction Class -> Dep4Func 
extractClassDepForTopLevelFunction f = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
  where ret = toplevelfunc_ret f 
        args = toplevelfunc_args f 

-- | 
mkModuleDepRaw :: Class -> [Class] 
mkModuleDepRaw c = (nub . filter (/= c) . mapMaybe (returnDependency.extractClassDep) . class_funcs) c
  where class_funcs = classFuncs


-- | 
mkModuleDepHighNonSource :: Class -> [Class] 
mkModuleDepHighNonSource c = 
  let fs = classFuncs c 
      pkgname = (cabal_pkgname . class_cabal) c 
      extclasses = (filter (\x-> x /= c && ((/= pkgname) . cabal_pkgname . class_cabal) x) . concatMap (argumentDependency.extractClassDep)) fs
      parents = class_parents c 
  in  nub (parents ++ extclasses) 


-- | 
mkModuleDepHighSource :: Class -> [Class] 
mkModuleDepHighSource c = 
  let fs = classFuncs c 
      pkgname = (cabal_pkgname . class_cabal) c 
  in  nub . filter (\x-> x /= c && not (x `elem` class_parents c) && (((== pkgname) . cabal_pkgname . class_cabal) x)) . concatMap (argumentDependency.extractClassDep) $ fs

-- | 
mkModuleDepCpp :: Class -> [Class] 
mkModuleDepCpp c = 
  let fs = classFuncs c 
  in  nub . filter (/= c)  $ 
        mapMaybe (returnDependency.extractClassDep) fs   
        ++ concatMap (argumentDependency.extractClassDep) fs
        ++ (class_parents c) 

-- | 
mkModuleDepFFI4One :: Class -> [Class] 
mkModuleDepFFI4One c = 
  let fs = classFuncs c 
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

                    
mkClassModule :: (Class->([Namespace],[String]))
              -> Class 
              -> ClassModule 
mkClassModule mkincheaders c = 
    let r = (ClassModule <$> getClassModuleBase  
                 <*> pure
                 <*> return . mkCIH mkincheaders
                 <*> highs_nonsource  
                 <*> raws 
                 <*> highs_source
                 <*> ffis 
            ) c
    in r 
    
  where highs_nonsource = map getClassModuleBase . mkModuleDepHighNonSource
        raws = map getClassModuleBase . mkModuleDepRaw 
        highs_source = map getClassModuleBase . mkModuleDepHighSource
        ffis = map getClassModuleBase . mkModuleDepFFI 

 
type PackageIncludeHeaders = (String,Class->([Namespace],[String])) -- ^ (package name,mkIncludeHeaders)
mkAll_ClassModules_CIH_TIH :: PackageIncludeHeaders
                        -> ([Class],[TopLevelFunction Class]) 
                        -> ([ClassModule],[ClassImportHeader],TopLevelImportHeader Class)
mkAll_ClassModules_CIH_TIH (pkgname,mkNSandIncHdrs) (cs,fs) = 
  let ms = map (mkClassModule mkNSandIncHdrs) cs 
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
      -- for toplevel 
      tl_cs1 = concatMap (argumentDependency . extractClassDepForTopLevelFunction) fs 
      tl_cs2 = mapMaybe (returnDependency . extractClassDepForTopLevelFunction) fs 
      tl_cs = nubBy ((==) `on` class_name) (tl_cs1 ++ tl_cs2)
      tl_cihs = catMaybes $ 
        foldr (\c acc-> (find (\x -> (class_name . cihClass) x == class_name c) cihs):acc) [] tl_cs 
      -- 
      tih = TopLevelImportHeader (pkgname ++ "TopLevel") tl_cihs fs 
  in (ms,cihs,tih)


mkHSBOOTCandidateList :: [ClassModule] -> [String]
mkHSBOOTCandidateList ms = nub (concatMap cmImportedModulesHighSource ms)
