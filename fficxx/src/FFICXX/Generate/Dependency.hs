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

import Data.Bifunctor (bimap)
import Data.Either (rights)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L (find, foldl', nub, nubBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import FFICXX.Generate.Name
  ( ffiClassName,
    getClassModuleBase,
    getTClassModuleBase,
    hsClassName,
  )
import FFICXX.Generate.Type.Cabal
  ( AddCInc,
    AddCSrc,
    CabalName (..),
    cabal_cheaderprefix,
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
    ClassSubmoduleType (..),
    PackageConfig (..),
    TemplateClassImportHeader (..),
    TemplateClassModule (..),
    TemplateClassSubmoduleType (..),
    TopLevelImportHeader (..),
    UClassSubmodule,
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
        else L.nub (ps <> (concatMap class_allparents ps))

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
mkDaughterSelfMap = L.foldl' worker M.empty
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

mkDepFFI :: Class -> [UClassSubmodule]
mkDepFFI cls =
  let ps = map Right (class_allparents cls)
      alldeps' = concatMap go ps <> go (Right cls)
      depSelf = Right (CSTRawType, cls)
   in depSelf : (fmap (bimap (TCSTTemplate,) (CSTRawType,)) $ L.nub $ filter (/= Right cls) alldeps')
  where
    go (Right c) =
      let fs = class_funcs c
          vs = class_vars c
          tmfs = class_tmpl_funcs c
       in concatMap (returnDependency . extractClassDep) fs
            <> concatMap (argumentDependency . extractClassDep) fs
            <> concatMap (classFromArg . unVariable) vs
            <> concatMap (returnDependency . extractClassDep4TmplMemberFun) tmfs
            <> concatMap (argumentDependency . extractClassDep4TmplMemberFun) tmfs
    go (Left t) =
      let fs = tclass_funcs t
       in concatMap (returnDependency . extractClassDepForTmplFun) fs
            <> concatMap (argumentDependency . extractClassDepForTmplFun) fs

-- For raws:
-- NOTE: Q: Why returnDependency for RawTypes?
--       A: Difference between argument and return:
--          for a member function f,
--          we have (f :: (IA a, IB b) => a -> b -> IO C
--          return class is concrete and argument class is constraint.
calculateDependency :: UClassSubmodule -> [UClassSubmodule]
calculateDependency (Left (typ, tcl)) = raws <> inplaces
  where
    raws' =
      L.nub $
        filter (/= Left tcl) $
          concatMap (returnDependency . extractClassDepForTmplFun) $
            tclass_funcs tcl
    raws =
      case typ of
        TCSTTemplate ->
          fmap (bimap (TCSTTemplate,) (CSTRawType,)) raws'
        TCSTTH ->
          concatMap
            ( \case
                Left t -> [Left (TCSTTemplate, t)]
                Right c -> fmap (Right . (,c)) [CSTRawType, CSTCast, CSTInterface]
            )
            raws'
    inplaces =
      let fs = tclass_funcs tcl
       in fmap (bimap (TCSTTemplate,) (CSTInterface,)) $
            L.nub $
              filter (`isInSamePackageButNotInheritedBy` Left tcl) $
                concatMap (argumentDependency . extractClassDepForTmplFun) fs
calculateDependency (Right (CSTFFI, cls)) = mkDepFFI cls
calculateDependency (Right (CSTInterface, cls)) =
  let retDepClasses =
        concatMap (returnDependency . extractClassDep) (class_funcs cls)
          ++ concatMap (returnDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs cls)
      argDepClasses =
        concatMap (argumentDependency . extractClassDep) (class_funcs cls)
          ++ concatMap (argumentDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs cls)
      rawSelf = Right (CSTRawType, cls)
      raws =
        fmap (bimap (TCSTTemplate,) (CSTRawType,)) $ L.nub $ filter (/= Right cls) retDepClasses
      exts =
        let extclasses =
              filter (`isNotInSamePackageWith` Right cls) argDepClasses
            parents = map Right (class_parents cls)
         in fmap (bimap (TCSTTemplate,) (CSTInterface,)) $ L.nub (parents <> extclasses)
      inplaces =
        fmap (bimap (TCSTTemplate,) (CSTInterface,)) $
          L.nub $
            filter (`isInSamePackageButNotInheritedBy` Right cls) $ argDepClasses
   in rawSelf : (raws ++ exts ++ inplaces)
calculateDependency (Right (CSTCast, cls)) = [Right (CSTRawType, cls), Right (CSTInterface, cls)]
calculateDependency (Right (CSTImplementation, cls)) =
  let depsSelf =
        [ Right (CSTRawType, cls),
          Right (CSTFFI, cls),
          Right (CSTInterface, cls),
          Right (CSTCast, cls)
        ]
      dsFFI = fmap (bimap snd snd) $ mkDepFFI cls
      dsParents = L.nub $ map Right $ class_allparents cls
      dsNonParents = filter (not . (flip elem dsParents)) dsFFI

      deps =
        concatMap
          ( \case
              Left tcl -> [Left (TCSTTemplate, tcl)]
              Right cls ->
                [ Right (CSTRawType, cls),
                  Right (CSTCast, cls),
                  Right (CSTInterface, cls)
                ]
          )
          (dsNonParents <> dsParents)
   in depsSelf <> deps

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

-- |
mkModuleDepCpp :: Either TemplateClass Class -> [Either TemplateClass Class]
mkModuleDepCpp y@(Right c) =
  let fs = class_funcs c
      vs = class_vars c
      tmfs = class_tmpl_funcs c
   in L.nub . filter (/= y) $
        concatMap (returnDependency . extractClassDep) fs
          <> concatMap (argumentDependency . extractClassDep) fs
          <> concatMap (classFromArg . unVariable) vs
          <> concatMap (returnDependency . extractClassDep4TmplMemberFun) tmfs
          <> concatMap (argumentDependency . extractClassDep4TmplMemberFun) tmfs
          <> getparents y
mkModuleDepCpp y@(Left t) =
  let fs = tclass_funcs t
   in L.nub . filter (/= y) $
        concatMap (returnDependency . extractClassDepForTmplFun) fs
          <> concatMap (argumentDependency . extractClassDepForTmplFun) fs
          <> getparents y

-- | Find module-level dependency per each toplevel function/template function.
mkTopLevelDep :: TopLevel -> [UClassSubmodule]
mkTopLevelDep (TLOrdinary f) =
  let dep4func = extractClassDepForTLOrdinary f
      allDeps = returnDependency dep4func ++ argumentDependency dep4func
      mkTags (Left tcl) = [Left (TCSTTemplate, tcl)]
      mkTags (Right cls) = fmap (Right . (,cls)) [CSTRawType, CSTCast, CSTInterface]
   in concatMap mkTags allDeps
mkTopLevelDep (TLTemplate f) =
  let dep4func = extractClassDepForTLTemplate f
      allDeps = returnDependency dep4func ++ argumentDependency dep4func
      mkTags (Left tcl) = [Left (TCSTTemplate, tcl)]
      mkTags (Right cls) = fmap (Right . (,cls)) [CSTRawType, CSTCast, CSTInterface]
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
      cmImportedSubmodulesForInterface = calculateDependency $ Right (CSTInterface, c),
      cmImportedSubmodulesForFFI = calculateDependency $ Right (CSTFFI, c),
      cmImportedSubmodulesForCast = calculateDependency $ Right (CSTCast, c),
      cmImportedSubmodulesForImplementation = calculateDependency $ Right (CSTImplementation, c),
      cmExtraImport = fromMaybe [] (lookup (class_name c) extra)
    }

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
      cihs = L.nubBy cmpfunc (map cmCIH ms)
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

-- for now
mkHsBootCandidateList :: [ClassModule] -> [ClassModule]
mkHsBootCandidateList ms = []

{-
  let -- get only class dependencies, not template classes.
      cs = rights (concatMap cmImportedModulesInplace ms)
      candidateModBases = fmap getClassModuleBase cs
   in filter (\m -> cmModule m `elem` candidateModBases) ms
-}

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
      extheaders = L.nub . map ((<> "Type.h") . unCabalName . getPkgName) $ extclasses
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
      tl_cs = L.nubBy ((==) `on` either tclass_name ffiClassName) (tl_cs1 <> tl_cs2)
      -- NOTE: Select only class dependencies in the current package.
      -- TODO: This is clearly not a good impl. we need to look into this again
      --       after reconsidering multi-package generation.
      tl_cihs = catMaybes (foldr fn [] tl_cs)
        where
          fn c ys =
            let y = L.find (\x -> (ffiClassName . cihClass) x == getFFIName c) cihs
             in y : ys
      -- NOTE: The remaining class dependencies outside the current package
      extclasses = filter ((/= pkgname) . getPkgName) tl_cs
      extheaders =
        map HdrName $
          L.nub $
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
