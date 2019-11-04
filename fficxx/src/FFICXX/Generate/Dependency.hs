{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Dependency
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Dependency where

--
-- fficxx generates one module per one C++ class, and C++ class depends on other classes,
-- so we need to import other modules corresponding to C++ classes in the dependency list.
-- Calculating the import list from dependency graph is what this module does.

-- Previously, we have only `Class` type, but added `TemplateClass` recently. Therefore
-- we have to calculate dependency graph for both types of classes. So we needed to change
-- `Class` to `Either TemplateClass Class` in many of routines that calculates module import
-- list.

-- `Dep4Func` contains a list of classes (both ordinary and template types) that is needed
-- for the definition of a member function.
-- The goal of `extractClassDep...` functions are to extract Dep4Func, and from the definition
-- of a class or a template class, we get a list of `Dep4Func`s and then we deduplicate the
-- dependency class list and finally get the import list for the module corresponding to
-- a given class.
--

import           Data.Either                (rights)
import           Data.Function              (on)
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid                ((<>))
import           System.FilePath
--
import           FFICXX.Generate.Name       (ffiClassName,hsClassName,hsTemplateClassName)
import           FFICXX.Generate.Type.Cabal (AddCInc,AddCSrc,CabalName(..)
                                            ,cabal_moduleprefix,cabal_pkgname
                                            ,cabal_cheaderprefix,unCabalName)
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Config (ModuleUnit(..)
                                             ,ModuleUnitImports(..),emptyModuleUnitImports
                                             ,ModuleUnitMap(..))
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface


-- utility functions

getcabal = either tclass_cabal class_cabal

getparents = either (const []) (map Right . class_parents)

-- TODO: replace tclass_name with appropriate FFI name when supported.
getFFIName = either tclass_name ffiClassName


getPkgName :: Either TemplateClass Class -> CabalName
getPkgName = cabal_pkgname . getcabal


-- |
extractClassFromType :: Types -> [Either TemplateClass Class]
extractClassFromType Void                     = []
extractClassFromType SelfType                 = []
extractClassFromType (CT _ _)                 = []
extractClassFromType (CPT (CPTClass c) _)     = [Right c]
extractClassFromType (CPT (CPTClassRef c) _)  = [Right c]
extractClassFromType (CPT (CPTClassCopy c) _) = [Right c]
extractClassFromType (CPT (CPTClassMove c) _) = [Right c]
extractClassFromType (TemplateApp (TemplateAppInfo t p _)) =
  (Left t): case p of
              TArg_Class c -> [Right c]
              _            -> []
extractClassFromType (TemplateAppRef (TemplateAppInfo t p _))   =
  (Left t): case p of
              TArg_Class c -> [Right c]
              _            -> []
extractClassFromType (TemplateAppMove (TemplateAppInfo t p _))   =
  (Left t): case p of
              TArg_Class c -> [Right c]
              _            -> []
extractClassFromType (TemplateType t)         = [Left t]
extractClassFromType (TemplateParam _)        = []
extractClassFromType (TemplateParamPointer _) = []


class_allparents :: Class -> [Class]
class_allparents c = let ps = class_parents c
                     in  if null ps
                           then []
                           else nub (ps <> (concatMap class_allparents ps))


getClassModuleBase :: Class -> String
getClassModuleBase = (<.>) <$> (cabal_moduleprefix.class_cabal) <*> (fst.hsClassName)

getTClassModuleBase :: TemplateClass -> String
getTClassModuleBase = (<.>) <$> (cabal_moduleprefix.tclass_cabal) <*> (fst.hsTemplateClassName)


-- | Daughter map not including itself
mkDaughterMap :: [Class] -> DaughterMap
mkDaughterMap = foldl mkDaughterMapWorker M.empty
  where mkDaughterMapWorker m c = let ps = map getClassModuleBase (class_allparents c)
                                  in  foldl (addmeToYourDaughterList c) m ps
        addmeToYourDaughterList c m p = let f Nothing = Just [c]
                                            f (Just cs)  = Just (c:cs)
                                        in  M.alter f p m



-- | Daughter Map including itself as a daughter
mkDaughterSelfMap :: [Class] -> DaughterMap
mkDaughterSelfMap = foldl' worker M.empty
  where worker m c = let ps = map getClassModuleBase (c:class_allparents c)
                     in  foldl (addToList c) m ps
        addToList c m p = let f Nothing = Just [c]
                              f (Just cs)  = Just (c:cs)
                          in  M.alter f p m



-- | class dependency for a given function
data Dep4Func = Dep4Func { returnDependency :: [Either TemplateClass Class]
                         , argumentDependency :: [Either TemplateClass Class] }


-- |
extractClassDep :: Function -> Dep4Func
extractClassDep (Constructor args _)  =
    Dep4Func [] (concatMap (extractClassFromType.fst) args)
extractClassDep (Virtual ret _ args _) =
    Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType.fst) args)
extractClassDep (NonVirtual ret _ args _) =
    Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType.fst) args)
extractClassDep (Static ret _ args _) =
    Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType.fst) args)
extractClassDep (Destructor _) =
    Dep4Func [] []


extractClassDepForTmplFun :: TemplateFunction -> Dep4Func
extractClassDepForTmplFun (TFun ret  _ _ args _) =
    Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType.fst) args)
extractClassDepForTmplFun (TFunNew args _) =
    Dep4Func [] (concatMap (extractClassFromType.fst) args)
extractClassDepForTmplFun TFunDelete =
    Dep4Func [] []


extractClassDep4TmplMemberFun :: TemplateMemberFunction -> Dep4Func
extractClassDep4TmplMemberFun (TemplateMemberFunction {..}) =
  Dep4Func (extractClassFromType tmf_ret) (concatMap (extractClassFromType.fst) tmf_args)



extractClassDepForTopLevelFunction :: TopLevelFunction -> Dep4Func
extractClassDepForTopLevelFunction f =
    Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType.fst) args)
  where ret = case f of
                TopLevelFunction {..} -> toplevelfunc_ret
                TopLevelVariable {..} -> toplevelvar_ret
        args = case f of
                 TopLevelFunction {..} -> toplevelfunc_args
                 TopLevelVariable {..} -> []


-- TODO: Confirm the answer below is correct.
-- NOTE: Q: Why returnDependency only?
--       A: Difference between argument and return:
--          for a member function f,
--          we have (f :: (IA a, IB b) => a -> b -> IO C
--          return class is concrete and argument class is constraint.
mkModuleDepRaw :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepRaw x@(Right c) =
  nub $
    filter (/= x) $
         concatMap (returnDependency . extractClassDep) (class_funcs c)
      ++ concatMap (returnDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
mkModuleDepRaw x@(Left t) =
  (nub . filter (/= x) . concatMap (returnDependency.extractClassDepForTmplFun) . tclass_funcs) t



isNotInSamePackageWith
  :: Either TemplateClass Class
  -> Either TemplateClass Class
  -> Bool
isNotInSamePackageWith x y = (x /= y) && (getPkgName x /= getPkgName y)

-- x is in the sam
isInSamePackageButNotInheritedBy
  :: Either TemplateClass Class -- ^ y
  -> Either TemplateClass Class -- ^ x
  -> Bool
isInSamePackageButNotInheritedBy x y =
  x /= y && not (x `elem` getparents y) && (getPkgName x == getPkgName y)


-- TODO: Confirm the following answer
-- NOTE: Q: why returnDependency is not considered?
--       A: See explanation in mkModuleDepRaw
mkModuleDepHighNonSource :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepHighNonSource y@(Right c) =
  let extclasses = filter (`isNotInSamePackageWith` y) $
                        concatMap (argumentDependency.extractClassDep) (class_funcs c)
                     ++ concatMap (argumentDependency.extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
      parents = map Right (class_parents c)
  in  nub (parents <> extclasses)
mkModuleDepHighNonSource y@(Left t) =
  let fs = tclass_funcs t
      extclasses = filter (`isNotInSamePackageWith` y) $
                     concatMap (argumentDependency.extractClassDepForTmplFun) fs
  in  nub extclasses


-- TODO: Confirm the following answer
-- NOTE: Q: why returnDependency is not considered?
--       A: See explanation in mkModuleDepRaw
mkModuleDepHighSource :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepHighSource y@(Right c) =
  nub $
    filter (`isInSamePackageButNotInheritedBy` y) $
         concatMap (argumentDependency . extractClassDep) (class_funcs c)
      ++ concatMap (argumentDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
mkModuleDepHighSource y@(Left t) =
  let fs = tclass_funcs t
  in nub $
       filter (`isInSamePackageButNotInheritedBy` y) $
         concatMap (argumentDependency . extractClassDepForTmplFun) fs

-- |
mkModuleDepCpp :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepCpp y@(Right c) =
  let fs = class_funcs c
      vs = class_vars c
      tmfs = class_tmpl_funcs c
  in  nub . filter (/= y)  $
           concatMap (returnDependency.extractClassDep) fs
        <> concatMap (argumentDependency.extractClassDep) fs
        <> concatMap (extractClassFromType . var_type) vs
        <> concatMap (returnDependency.extractClassDep4TmplMemberFun) tmfs
        <> concatMap (argumentDependency.extractClassDep4TmplMemberFun) tmfs
        <> getparents y
mkModuleDepCpp y@(Left t) =
  let fs = tclass_funcs t
  in  nub . filter (/= y)  $
           concatMap (returnDependency.extractClassDepForTmplFun) fs
        <> concatMap (argumentDependency.extractClassDepForTmplFun) fs
        <> getparents y

-- |
mkModuleDepFFI1 :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepFFI1 (Right c) = let fs = class_funcs c
                                vs = class_vars c
                                tmfs = class_tmpl_funcs c
                            in    concatMap (returnDependency.extractClassDep) fs
                               <> concatMap (argumentDependency.extractClassDep) fs
                               <> concatMap (extractClassFromType . var_type) vs
                               <> concatMap (returnDependency.extractClassDep4TmplMemberFun) tmfs
                               <> concatMap (argumentDependency.extractClassDep4TmplMemberFun) tmfs
mkModuleDepFFI1 (Left t)  = let fs = tclass_funcs t
                            in    concatMap (returnDependency.extractClassDepForTmplFun) fs
                               <> concatMap (argumentDependency.extractClassDepForTmplFun) fs

-- |
mkModuleDepFFI :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepFFI y@(Right c) =
  let ps = map Right (class_allparents c)
      alldeps' = (concatMap mkModuleDepFFI1 ps) <> mkModuleDepFFI1 y
  in nub (filter (/= y) alldeps')
mkModuleDepFFI (Left _) = []


mkClassModule :: (ModuleUnit -> ModuleUnitImports)
              -> [(String,[String])]
              -> Class
              -> ClassModule
mkClassModule getImports extra c =
    ClassModule {
      cmModule = getClassModuleBase c
    , cmClass = [c]
    , cmCIH = map (mkCIH getImports) [c]
    , cmImportedModulesHighNonSource = highs_nonsource
    , cmImportedModulesRaw =raws
    , cmImportedModulesHighSource = highs_source
    , cmImportedModulesForFFI = ffis
    , cmExtraImport = extraimports
    }
  where highs_nonsource = mkModuleDepHighNonSource (Right c)
        raws            = mkModuleDepRaw (Right c)
        highs_source    = mkModuleDepHighSource (Right c)
        ffis            = mkModuleDepFFI (Right c)
        extraimports = fromMaybe [] (lookup (class_name c) extra)


findModuleUnitImports :: ModuleUnitMap -> ModuleUnit -> ModuleUnitImports
findModuleUnitImports m u =
  fromMaybe emptyModuleUnitImports (HM.lookup u (unModuleUnitMap m))


mkTCM :: (TemplateClass,HeaderName) -> TemplateClassModule
mkTCM (t,hdr) = TCM  (getTClassModuleBase t) [t] [TCIH t hdr]


mkPackageConfig
  :: (CabalName, ModuleUnit -> ModuleUnitImports) -- ^ (package name,getImports)
  -> ([Class],[TopLevelFunction],[(TemplateClass,HeaderName)],[(String,[String])])
  -> [AddCInc]
  -> [AddCSrc]
  -> PackageConfig
mkPackageConfig (pkgname,getImports) (cs,fs,ts,extra) acincs acsrcs =
  let ms = map (mkClassModule getImports extra) cs
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (concatMap cmCIH ms)
      --
      tih = mkTIH pkgname getImports cihs fs
      tcms = map mkTCM ts
      tcihs = concatMap tcmTCIH tcms
  in PkgConfig {
       pcfg_classModules = ms
     , pcfg_classImportHeaders = cihs
     , pcfg_topLevelImportHeader = tih
     , pcfg_templateClassModules = tcms
     , pcfg_templateClassImportHeaders = tcihs
     , pcfg_additional_c_incs = acincs
     , pcfg_additional_c_srcs = acsrcs
     }


-- TODO: change [String] to Set String
mkHSBOOTCandidateList :: [ClassModule] -> [String]
mkHSBOOTCandidateList ms =
  let
    -- get only class dependencies, not template classes.
    cs = rights (concatMap cmImportedModulesHighSource ms)
  in
    nub (map getClassModuleBase cs)



-- |
mkPkgHeaderFileName ::Class -> HeaderName
mkPkgHeaderFileName c =
    HdrName (   (cabal_cheaderprefix.class_cabal) c
            <>  fst (hsClassName c)
            <.> "h"
            )

-- |
mkPkgCppFileName ::Class -> String
mkPkgCppFileName c =
        (cabal_cheaderprefix.class_cabal) c
    <>  fst (hsClassName c)
    <.> "cpp"

-- |
mkPkgIncludeHeadersInH :: Class -> [HeaderName]
mkPkgIncludeHeadersInH c =
    let pkgname = (cabal_pkgname . class_cabal) c
        extclasses = filter ((/= pkgname) . getPkgName) . mkModuleDepCpp $ Right c
        extheaders = nub . map ((<>"Type.h") . unCabalName . getPkgName) $ extclasses
    in map mkPkgHeaderFileName (class_allparents c) <> map HdrName extheaders



-- |
mkPkgIncludeHeadersInCPP :: Class -> [HeaderName]
mkPkgIncludeHeadersInCPP = map mkPkgHeaderFileName . rights . mkModuleDepCpp . Right


-- |
mkCIH :: (ModuleUnit -> ModuleUnitImports)  -- ^ (mk namespace and include headers)
      -> Class
      -> ClassImportHeader
mkCIH getImports c =
  ClassImportHeader {
    cihClass                    = c
  , cihSelfHeader               = mkPkgHeaderFileName c
  , cihNamespace                = (muimports_namespaces . getImports . MU_Class . class_name) c
  , cihSelfCpp                  = mkPkgCppFileName c
  , cihImportedClasses          = mkModuleDepCpp (Right c)
  , cihIncludedHPkgHeadersInH   = mkPkgIncludeHeadersInH c
  , cihIncludedHPkgHeadersInCPP = mkPkgIncludeHeadersInCPP c
  , cihIncludedCPkgHeaders      = (muimports_headers . getImports . MU_Class . class_name) c
  }

-- | for top-level
mkTIH
  :: CabalName
  -> (ModuleUnit -> ModuleUnitImports)
  -> [ClassImportHeader]
  -> [TopLevelFunction]
  -> TopLevelImportHeader
mkTIH pkgname getImports cihs fs =
  let tl_cs1 = concatMap (argumentDependency . extractClassDepForTopLevelFunction) fs
      tl_cs2 = concatMap (returnDependency . extractClassDepForTopLevelFunction) fs
      tl_cs = nubBy ((==) `on` either tclass_name ffiClassName) (tl_cs1 <> tl_cs2)
      -- NOTE: Select only class dependencies in the current package.
      -- TODO: This is clearly not a good impl. we need to look into this again
      --       after reconsidering multi-package generation.
      tl_cihs = catMaybes (foldr fn [] tl_cs)
         where
           fn c ys =
             let y = find (\x -> (ffiClassName . cihClass) x == getFFIName c) cihs
             in y:ys
      -- NOTE: The remaining class dependencies outside the current package
      extclasses = filter ((/= pkgname) . getPkgName) tl_cs
      extheaders = map HdrName $
                     nub $
                       map ((<>"Type.h") . unCabalName . getPkgName)  extclasses

  in
     TopLevelImportHeader {
       tihHeaderFileName = unCabalName pkgname <> "TopLevel"
     , tihClassDep = tl_cihs
     , tihExtraClassDep = extclasses
     , tihFuncs = fs
     , tihNamespaces        = muimports_namespaces (getImports MU_TopLevel)
     , tihExtraHeadersInH   = extheaders
     , tihExtraHeadersInCPP = muimports_headers (getImports MU_TopLevel)
     }
