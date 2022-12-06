{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module FFICXX.Generate.Dependency where

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

import Data.Either (rights)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (find, foldl', nub, nubBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import FFICXX.Generate.Name
  ( ClassModuleType (..),
    TemplateClassModuleType (..),
    ffiClassName,
    getClassModuleBase,
    getTClassModuleBase,
    hsClassName,
    hsTemplateClassName,
  )
import FFICXX.Generate.Type.Cabal
  ( AddCInc,
    AddCSrc,
    CabalName (..),
    cabal_cheaderprefix,
    cabal_moduleprefix,
    cabal_pkgname,
    unCabalName,
  )
import FFICXX.Generate.Type.Class
  ( Arg (..),
    CPPTypes (..),
    Class (..),
    DaughterMap,
    Function (..),
    TLOrdinary (..),
    TLTemplate (..),
    TemplateAppInfo (..),
    TemplateArgType (TArg_Class),
    TemplateClass (..),
    TemplateFunction (..),
    TemplateMemberFunction (..),
    TopLevel (..),
    Types (..),
    Variable (unVariable),
    argsFromOpExp,
    filterTLOrdinary,
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    emptyModuleUnitImports,
  )
import FFICXX.Generate.Type.Module
  ( ClassImportHeader (..),
    ClassModule (..),
    PackageConfig (..),
    TemplateClassImportHeader (..),
    TemplateClassModule (..),
    TopLevelImportHeader (..),
  )
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import System.FilePath ((<.>))

-- utility functions

getcabal = either tclass_cabal class_cabal

getparents = either (const []) (map Right . class_parents)

-- TODO: replace tclass_name with appropriate FFI name when supported.
getFFIName = either tclass_name ffiClassName

getPkgName :: Either TemplateClass Class -> CabalName
getPkgName = cabal_pkgname . getcabal

-- |
extractClassFromType :: Types -> [Either TemplateClass Class]
extractClassFromType Void = []
extractClassFromType SelfType = []
extractClassFromType (CT _ _) = []
extractClassFromType (CPT (CPTClass c) _) = [Right c]
extractClassFromType (CPT (CPTClassRef c) _) = [Right c]
extractClassFromType (CPT (CPTClassCopy c) _) = [Right c]
extractClassFromType (CPT (CPTClassMove c) _) = [Right c]
extractClassFromType (TemplateApp (TemplateAppInfo t ps _)) =
  Left t : (map Right $ mapMaybe (\case TArg_Class c -> Just c; _ -> Nothing) ps)
extractClassFromType (TemplateAppRef (TemplateAppInfo t ps _)) =
  Left t : (map Right $ mapMaybe (\case TArg_Class c -> Just c; _ -> Nothing) ps)
extractClassFromType (TemplateAppMove (TemplateAppInfo t ps _)) =
  Left t : (map Right $ mapMaybe (\case TArg_Class c -> Just c; _ -> Nothing) ps)
extractClassFromType (TemplateType t) = [Left t]
extractClassFromType (TemplateParam _) = []
extractClassFromType (TemplateParamPointer _) = []

classFromArg :: Arg -> [Either TemplateClass Class]
classFromArg = extractClassFromType . arg_type

class_allparents :: Class -> [Class]
class_allparents c =
  let ps = class_parents c
   in if null ps
        then []
        else nub (ps <> (concatMap class_allparents ps))

-- | Daughter map not including itself
mkDaughterMap :: [Class] -> DaughterMap
mkDaughterMap = foldl mkDaughterMapWorker M.empty
  where
    mkDaughterMapWorker m c =
      let ps = map getClassModuleBase (class_allparents c)
       in foldl (addmeToYourDaughterList c) m ps
    addmeToYourDaughterList c m p =
      let f Nothing = Just [c]
          f (Just cs) = Just (c : cs)
       in M.alter f p m

-- | Daughter Map including itself as a daughter
mkDaughterSelfMap :: [Class] -> DaughterMap
mkDaughterSelfMap = foldl' worker M.empty
  where
    worker m c =
      let ps = map getClassModuleBase (c : class_allparents c)
       in foldl (addToList c) m ps
    addToList c m p =
      let f Nothing = Just [c]
          f (Just cs) = Just (c : cs)
       in M.alter f p m

-- | class dependency for a given function
data Dep4Func = Dep4Func
  { returnDependency :: [Either TemplateClass Class],
    argumentDependency :: [Either TemplateClass Class]
  }

-- |
extractClassDep :: Function -> Dep4Func
extractClassDep (Constructor args _) =
  Dep4Func [] (concatMap classFromArg args)
extractClassDep (Virtual ret _ args _) =
  Dep4Func (extractClassFromType ret) (concatMap classFromArg args)
extractClassDep (NonVirtual ret _ args _) =
  Dep4Func (extractClassFromType ret) (concatMap classFromArg args)
extractClassDep (Static ret _ args _) =
  Dep4Func (extractClassFromType ret) (concatMap classFromArg args)
extractClassDep (Destructor _) =
  Dep4Func [] []

-- |
extractClassDepForTmplFun :: TemplateFunction -> Dep4Func
extractClassDepForTmplFun (TFun ret _ _ args) =
  Dep4Func (extractClassFromType ret) (concatMap classFromArg args)
extractClassDepForTmplFun (TFunNew args _) =
  Dep4Func [] (concatMap classFromArg args)
extractClassDepForTmplFun TFunDelete =
  Dep4Func [] []
extractClassDepForTmplFun (TFunOp ret _ e) =
  Dep4Func (extractClassFromType ret) (concatMap classFromArg $ argsFromOpExp e)

-- |
extractClassDep4TmplMemberFun :: TemplateMemberFunction -> Dep4Func
extractClassDep4TmplMemberFun (TemplateMemberFunction {..}) =
  Dep4Func (extractClassFromType tmf_ret) (concatMap classFromArg tmf_args)

-- |
extractClassDepForTLOrdinary :: TLOrdinary -> Dep4Func
extractClassDepForTLOrdinary f =
  Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType . arg_type) args)
  where
    ret = case f of
      TopLevelFunction {..} -> toplevelfunc_ret
      TopLevelVariable {..} -> toplevelvar_ret
    args = case f of
      TopLevelFunction {..} -> toplevelfunc_args
      TopLevelVariable {} -> []

-- |
extractClassDepForTLTemplate :: TLTemplate -> Dep4Func
extractClassDepForTLTemplate f =
  Dep4Func (extractClassFromType ret) (concatMap (extractClassFromType . arg_type) args)
  where
    ret = topleveltfunc_ret f
    args = topleveltfunc_args f

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
  (nub . filter (/= x) . concatMap (returnDependency . extractClassDepForTmplFun) . tclass_funcs) t

-- |
isNotInSamePackageWith ::
  Either TemplateClass Class ->
  Either TemplateClass Class ->
  Bool
isNotInSamePackageWith x y = (x /= y) && (getPkgName x /= getPkgName y)

-- x is in the sam
isInSamePackageButNotInheritedBy ::
  -- | y
  Either TemplateClass Class ->
  -- | x
  Either TemplateClass Class ->
  Bool
isInSamePackageButNotInheritedBy x y =
  x /= y && not (x `elem` getparents y) && (getPkgName x == getPkgName y)

-- TODO: Confirm the following answer
-- NOTE: Q: why returnDependency is not considered?
--       A: See explanation in mkModuleDepRaw
mkModuleDepExternal :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepExternal y@(Right c) =
  let extclasses =
        filter (`isNotInSamePackageWith` y) $
          concatMap (argumentDependency . extractClassDep) (class_funcs c)
            ++ concatMap (argumentDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
      parents = map Right (class_parents c)
   in nub (parents <> extclasses)
mkModuleDepExternal y@(Left t) =
  let fs = tclass_funcs t
      extclasses =
        filter (`isNotInSamePackageWith` y) $
          concatMap (argumentDependency . extractClassDepForTmplFun) fs
   in nub extclasses

-- NOTE: Q: why returnDependency is not considered?
--       A: See explanation in mkModuleDepRaw
mkModuleDepInplace :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepInplace y@(Right c) =
  nub $
    filter (`isInSamePackageButNotInheritedBy` y) $
      concatMap (argumentDependency . extractClassDep) (class_funcs c)
        ++ concatMap (argumentDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
mkModuleDepInplace y@(Left t) =
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
   in nub . filter (/= y) $
        concatMap (returnDependency . extractClassDep) fs
          <> concatMap (argumentDependency . extractClassDep) fs
          <> concatMap (classFromArg . unVariable) vs
          <> concatMap (returnDependency . extractClassDep4TmplMemberFun) tmfs
          <> concatMap (argumentDependency . extractClassDep4TmplMemberFun) tmfs
          <> getparents y
mkModuleDepCpp y@(Left t) =
  let fs = tclass_funcs t
   in nub . filter (/= y) $
        concatMap (returnDependency . extractClassDepForTmplFun) fs
          <> concatMap (argumentDependency . extractClassDepForTmplFun) fs
          <> getparents y

-- |
mkModuleDepFFI1 :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepFFI1 (Right c) =
  let fs = class_funcs c
      vs = class_vars c
      tmfs = class_tmpl_funcs c
   in concatMap (returnDependency . extractClassDep) fs
        <> concatMap (argumentDependency . extractClassDep) fs
        <> concatMap (classFromArg . unVariable) vs
        <> concatMap (returnDependency . extractClassDep4TmplMemberFun) tmfs
        <> concatMap (argumentDependency . extractClassDep4TmplMemberFun) tmfs
mkModuleDepFFI1 (Left t) =
  let fs = tclass_funcs t
   in concatMap (returnDependency . extractClassDepForTmplFun) fs
        <> concatMap (argumentDependency . extractClassDepForTmplFun) fs

-- |
mkModuleDepFFI :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepFFI y@(Right c) =
  let ps = map Right (class_allparents c)
      alldeps' = (concatMap mkModuleDepFFI1 ps) <> mkModuleDepFFI1 y
   in nub (filter (/= y) alldeps')
mkModuleDepFFI (Left _) = []

-- | Find module-level dependency per each toplevel function/template function.
mkTopLevelDep ::
  TopLevel ->
  [ Either
      (TemplateClassModuleType, TemplateClass)
      (ClassModuleType, Class)
  ]
mkTopLevelDep (TLOrdinary f) =
  let dep4func = extractClassDepForTLOrdinary f
      allDeps = returnDependency dep4func ++ argumentDependency dep4func
      mkTags (Left tcl) = [Left (TCMTTemplate, tcl)]
      mkTags (Right cls) = fmap (Right . (,cls)) [CMTRawType, CMTCast, CMTInterface]
   in concatMap mkTags allDeps
mkTopLevelDep (TLTemplate f) =
  let dep4func = extractClassDepForTLTemplate f
      allDeps = returnDependency dep4func ++ argumentDependency dep4func
      mkTags (Left tcl) = [Left (TCMTTemplate, tcl)]
      mkTags (Right cls) = fmap (Right . (,cls)) [CMTRawType, CMTCast, CMTInterface]
   in concatMap mkTags allDeps

-- |
mkClassModule ::
  (ModuleUnit -> ModuleUnitImports) ->
  [(String, [String])] ->
  Class ->
  ClassModule
mkClassModule getImports extra c =
  ClassModule
    { cmModule = getClassModuleBase c,
      cmCIH = mkCIH getImports c,
      cmImportedModulesExternal = exts,
      cmImportedModulesRaw = raws,
      cmImportedModulesInplace = inplaces,
      cmImportedModulesFFI = ffis,
      cmExtraImport = extraimports
    }
  where
    exts = mkModuleDepExternal (Right c)
    raws = mkModuleDepRaw (Right c)
    inplaces = mkModuleDepInplace (Right c)
    ffis = mkModuleDepFFI (Right c)
    extraimports = fromMaybe [] (lookup (class_name c) extra)

-- |
findModuleUnitImports :: ModuleUnitMap -> ModuleUnit -> ModuleUnitImports
findModuleUnitImports m u =
  fromMaybe emptyModuleUnitImports (HM.lookup u (unModuleUnitMap m))

-- |
mkTCM ::
  TemplateClassImportHeader ->
  TemplateClassModule
mkTCM tcih =
  let t = tcihTClass tcih
   in TCM (getTClassModuleBase t) tcih

-- |
mkPackageConfig ::
  -- | (package name,getImports)
  (CabalName, ModuleUnit -> ModuleUnitImports) ->
  ([Class], [TopLevel], [TemplateClassImportHeader], [(String, [String])]) ->
  [AddCInc] ->
  [AddCSrc] ->
  PackageConfig
mkPackageConfig (pkgname, getImports) (cs, fs, ts, extra) acincs acsrcs =
  let ms = map (mkClassModule getImports extra) cs
      cmpfunc x y = class_name (cihClass x) == class_name (cihClass y)
      cihs = nubBy cmpfunc (map cmCIH ms)
      --
      tih = mkTIH pkgname getImports cihs fs
      tcms = map mkTCM ts
      tcihs = map tcmTCIH tcms
   in PkgConfig
        { pcfg_classModules = ms,
          pcfg_classImportHeaders = cihs,
          pcfg_topLevelImportHeader = tih,
          pcfg_templateClassModules = tcms,
          pcfg_templateClassImportHeaders = tcihs,
          pcfg_additional_c_incs = acincs,
          pcfg_additional_c_srcs = acsrcs
        }

mkHsBootCandidateList :: [ClassModule] -> [ClassModule]
mkHsBootCandidateList ms =
  let -- get only class dependencies, not template classes.
      cs = rights (concatMap cmImportedModulesInplace ms)
      candidateModBases = fmap getClassModuleBase cs
   in filter (\m -> cmModule m `elem` candidateModBases) ms

-- |
mkPkgHeaderFileName :: Class -> HeaderName
mkPkgHeaderFileName c =
  HdrName
    ( (cabal_cheaderprefix . class_cabal) c
        <> fst (hsClassName c)
        <.> "h"
    )

-- |
mkPkgCppFileName :: Class -> String
mkPkgCppFileName c =
  (cabal_cheaderprefix . class_cabal) c
    <> fst (hsClassName c)
    <.> "cpp"

-- |
mkPkgIncludeHeadersInH :: Class -> [HeaderName]
mkPkgIncludeHeadersInH c =
  let pkgname = (cabal_pkgname . class_cabal) c
      extclasses = filter ((/= pkgname) . getPkgName) . mkModuleDepCpp $ Right c
      extheaders = nub . map ((<> "Type.h") . unCabalName . getPkgName) $ extclasses
   in map mkPkgHeaderFileName (class_allparents c) <> map HdrName extheaders

-- |
mkPkgIncludeHeadersInCPP :: Class -> [HeaderName]
mkPkgIncludeHeadersInCPP = map mkPkgHeaderFileName . rights . mkModuleDepCpp . Right

-- |
mkCIH ::
  -- | (mk namespace and include headers)
  (ModuleUnit -> ModuleUnitImports) ->
  Class ->
  ClassImportHeader
mkCIH getImports c =
  ClassImportHeader
    { cihClass = c,
      cihSelfHeader = mkPkgHeaderFileName c,
      cihNamespace = (muimports_namespaces . getImports . MU_Class . class_name) c,
      cihSelfCpp = mkPkgCppFileName c,
      cihImportedClasses = mkModuleDepCpp (Right c),
      cihIncludedHPkgHeadersInH = mkPkgIncludeHeadersInH c,
      cihIncludedHPkgHeadersInCPP = mkPkgIncludeHeadersInCPP c,
      cihIncludedCPkgHeaders = (muimports_headers . getImports . MU_Class . class_name) c
    }

-- | for top-level
mkTIH ::
  CabalName ->
  (ModuleUnit -> ModuleUnitImports) ->
  [ClassImportHeader] ->
  [TopLevel] ->
  TopLevelImportHeader
mkTIH pkgname getImports cihs fs =
  let ofs = filterTLOrdinary fs
      tl_cs1 = concatMap (argumentDependency . extractClassDepForTLOrdinary) ofs
      tl_cs2 = concatMap (returnDependency . extractClassDepForTLOrdinary) ofs
      tl_cs = nubBy ((==) `on` either tclass_name ffiClassName) (tl_cs1 <> tl_cs2)
      -- NOTE: Select only class dependencies in the current package.
      -- TODO: This is clearly not a good impl. we need to look into this again
      --       after reconsidering multi-package generation.
      tl_cihs = catMaybes (foldr fn [] tl_cs)
        where
          fn c ys =
            let y = find (\x -> (ffiClassName . cihClass) x == getFFIName c) cihs
             in y : ys
      -- NOTE: The remaining class dependencies outside the current package
      extclasses = filter ((/= pkgname) . getPkgName) tl_cs
      extheaders =
        map HdrName $
          nub $
            map ((<> "Type.h") . unCabalName . getPkgName) extclasses
   in TopLevelImportHeader
        { tihHeaderFileName = unCabalName pkgname <> "TopLevel",
          tihClassDep = tl_cihs,
          tihExtraClassDep = extclasses,
          tihFuncs = fs,
          tihNamespaces = muimports_namespaces (getImports MU_TopLevel),
          tihExtraHeadersInH = extheaders,
          tihExtraHeadersInCPP = muimports_headers (getImports MU_TopLevel)
        }
