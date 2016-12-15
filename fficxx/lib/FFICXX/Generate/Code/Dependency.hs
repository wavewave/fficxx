{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Dependency
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.Dependency where

import           Control.Applicative
import           Data.Function (on)
import           Data.List 
import           Data.Maybe
import qualified Data.HashMap.Strict as HM
import           System.FilePath 
--
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface


-- | 
mkPkgHeaderFileName ::Class -> HeaderName
mkPkgHeaderFileName c = 
    HdrName ((cabal_cheaderprefix.class_cabal) c ++ class_name c <.> "h")

-- | 
mkPkgCppFileName ::Class -> String 
mkPkgCppFileName c = 
    (cabal_cheaderprefix.class_cabal) c ++ class_name c <.> "cpp"

-- | 
mkPkgIncludeHeadersInH :: Class -> [HeaderName]
mkPkgIncludeHeadersInH c =
    let pkgname = (cabal_pkgname . class_cabal) c
        extclasses = (filter ((/= pkgname) . cabal_pkgname . class_cabal) . mkModuleDepCpp) c
        extheaders = nub . map ((++"Type.h") .  cabal_pkgname . class_cabal) $ extclasses  
    in map mkPkgHeaderFileName (class_allparents c) ++ map HdrName extheaders

                           

-- | 
mkPkgIncludeHeadersInCPP :: Class -> [HeaderName]
mkPkgIncludeHeadersInCPP = map mkPkgHeaderFileName . mkModuleDepCpp 


-- | 
mkCIH :: (Class->([Namespace],[HeaderName]))  -- ^ (mk namespace and include headers)  
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


-- | 
extractClassDep :: Function -> Dep4Func 
extractClassDep (Constructor args _)  = Dep4Func Nothing (catMaybes (map (extractClassFromType.fst) args))
extractClassDep (Virtual ret _ args _) = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep (NonVirtual ret _ args _) =
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep (Static ret _ args _) = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
extractClassDep (Destructor _) = 
    Dep4Func Nothing [] 

extractClassDepForTopLevelFunction :: TopLevelFunction -> Dep4Func 
extractClassDepForTopLevelFunction f = 
    Dep4Func (extractClassFromType ret) (mapMaybe (extractClassFromType.fst) args)
  where ret = case f of 
                TopLevelFunction {..} -> toplevelfunc_ret
                TopLevelVariable {..} -> toplevelvar_ret
        args = case f of
                 TopLevelFunction {..} -> toplevelfunc_args
                 TopLevelVariable {..} -> [] 

-- | 
mkModuleDepRaw :: Class -> [Class] 
mkModuleDepRaw c = (nub . filter (/= c) . mapMaybe (returnDependency.extractClassDep) . class_funcs) c


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

                    
mkClassModule :: (Class->([Namespace],[HeaderName]))
              -> Class 
              -> ClassModule 
mkClassModule mkincheaders c = (ClassModule <$> getClassModuleBase  
                                            <*> pure
                                            <*> return . mkCIH mkincheaders
                                            <*> highs_nonsource  
                                            <*> raws 
                                            <*> highs_source
                                            <*> ffis
                               ) c
  where highs_nonsource = map getClassModuleBase . mkModuleDepHighNonSource
        raws = map getClassModuleBase . mkModuleDepRaw 
        highs_source = map getClassModuleBase . mkModuleDepHighSource
        ffis = map getClassModuleBase . mkModuleDepFFI


mkClassNSHeaderFromMap :: HM.HashMap String ([Namespace],[HeaderName]) -> Class -> ([Namespace],[HeaderName])
mkClassNSHeaderFromMap m c = fromMaybe ([],[]) (HM.lookup (class_name c) m)


mkTCM :: TemplateClass -> TemplateClassModule 
mkTCM t = TCM  (getTClassModuleBase t) [t]

mkAll_CM_CIH_TIH_TCM
  :: (String,Class->([Namespace],[HeaderName])) -- ^ (package name,mkIncludeHeaders)
  -> ([Class],[TopLevelFunction],[TemplateClass]) 
  -> ([ClassModule],[ClassImportHeader],TopLevelImportHeader,[TemplateClassModule])
mkAll_CM_CIH_TIH_TCM (pkgname,mkNSandIncHdrs) (cs,fs,ts) = 
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
      tcms = map mkTCM ts
  in (ms,cihs,tih,tcms)


mkHSBOOTCandidateList :: [ClassModule] -> [String]
mkHSBOOTCandidateList ms = nub (concatMap cmImportedModulesHighSource ms)
