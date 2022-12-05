module FFICXX.Generate.Dependency.Internal
  ( testfunc1,
    testfunc2,
  )
where

import Data.Either (rights)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (find, foldl', nub, nubBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import FFICXX.Generate.Name (ffiClassName, hsClassName, hsTemplateClassName)
import FFICXX.Generate.Type.Cabal
  ( AddCInc,
    AddCSrc,
    Cabal,
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

import FFICXX.Generate.Dependency
  ( Dep4Func (..),
    extractClassDep,
    extractClassDepForTmplFun,
    extractClassDep4TmplMemberFun,
    isNotInSamePackageWith,
    isInSamePackageButNotInheritedBy,
  )

-- everything: NonSource
testfunc1 :: Either TemplateClass Class -> [Either TemplateClass Class]
testfunc1 y@(Right c) =
  let extclasses =
        filter (`isNotInSamePackageWith` y) $
          concatMap (argumentDependency . extractClassDep) (class_funcs c)
            ++ concatMap (argumentDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
      parents = map Right (class_parents c)
   in nub (parents <> extclasses)
testfunc1 y@(Left t) =
  let fs = tclass_funcs t
      extclasses =
        filter (`isNotInSamePackageWith` y) $
          concatMap (argumentDependency . extractClassDepForTmplFun) fs
   in nub extclasses

-- Source
-- | Module imports with SOURCE pragma
-- TODO: Confirm the following answer
-- NOTE: Q: why returnDependency is not considered?
--       A: See explanation in mkModuleDepRaw
testfunc2 :: Either TemplateClass Class -> [Either TemplateClass Class]
testfunc2 y@(Right c) =
  nub $
    filter (`isInSamePackageButNotInheritedBy` y) $
      concatMap (argumentDependency . extractClassDep) (class_funcs c)
        ++ concatMap (argumentDependency . extractClassDep4TmplMemberFun) (class_tmpl_funcs c)
testfunc2 y@(Left t) =
  let fs = tclass_funcs t
   in nub $
        filter (`isInSamePackageButNotInheritedBy` y) $
          concatMap (argumentDependency . extractClassDepForTmplFun) fs
