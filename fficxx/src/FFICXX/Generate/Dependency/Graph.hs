{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FFICXX.Generate.Dependency.Graph where

import Data.Array (listArray)
import Data.Bifunctor (bimap)
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tree (flatten)
import Data.Tuple (swap)
import FFICXX.Generate.Dependency
  ( calculateDependency,
    class_allparents,
    mkTopLevelDep,
  )
import FFICXX.Generate.Name (subModuleName)
import FFICXX.Generate.Type.Class
  ( Class (..),
    TemplateClass (..),
    TopLevel (..),
  )
import FFICXX.Generate.Type.Module
  ( ClassSubmoduleType (..),
    DepCycles,
    TemplateClassSubmoduleType (..),
    UClass,
  )

-- TODO: Should be used everywhere.

-- | construct dependency graph
constructDepGraph ::
  -- | list of all classes, either template class or ordinary class.
  [UClass] ->
  -- | list of all top-level functions.
  [TopLevel] ->
  -- | (all submodules, [(submodule, submodule dependencies)])
  ([String], [(Int, [Int])])
constructDepGraph allclasses allTopLevels = (allSyms, depmap')
  where
    -- RawType dependency is trivial
    mkRawTypeDep :: Class -> (String, [String])
    mkRawTypeDep c =
      let rawtype = subModuleName (Right (CSTRawType, c))
       in (rawtype, [])
    -- FFI
    mkFFIDep :: Class -> (String, [String])
    mkFFIDep c =
      let ffi = subModuleName (Right (CSTFFI, c))
          depRawSelf = subModuleName (Right (CSTRawType, c))
          deps = fmap subModuleName $ calculateDependency $ Right (CSTFFI, c)
       in (ffi, depRawSelf : deps)
    mkInterfaceDep :: Class -> (String, [String])
    mkInterfaceDep c =
      let interface = subModuleName $ Right (CSTInterface, c)
          depRawSelf = subModuleName $ Right (CSTRawType, c)
          deps = fmap subModuleName $ calculateDependency $ Right (CSTInterface, c)
       in (interface, depRawSelf : deps)
    -- Cast
    mkCastDep :: Class -> (String, [String])
    mkCastDep c =
      let cast = subModuleName $ Right (CSTCast, c)
          deps = fmap subModuleName $ calculateDependency (Right (CSTCast, c))
       in (cast, deps)
    -- Implementation
    -- TODO: THIS IS INVOLVED! NEED TO REFACTOR THINGS OUT.
    mkImplementationDep :: Class -> (String, [String])
    mkImplementationDep c =
      let implementation = subModuleName $ Right (CSTImplementation, c)
          deps = fmap subModuleName $ calculateDependency $ Right (CSTImplementation, c)
       in (implementation, deps)
    -- Template Class part
    -- <TClass>.Template
    mkTemplateDep :: TemplateClass -> (String, [String])
    mkTemplateDep t =
      let template = subModuleName $ Left (TCSTTemplate, t)
          deps = fmap subModuleName $ calculateDependency $ Left (TCSTTemplate, t)
       in (template, deps)
    -- <TClass>.TH
    mkTHDep :: TemplateClass -> (String, [String])
    mkTHDep t =
      let th = subModuleName $ Left (TCSTTH, t)
          deps = fmap subModuleName $ calculateDependency $ Left (TCSTTH, t)
       in (th, deps)
    -- TopLevel
    topLevelDeps :: (String, [String])
    topLevelDeps =
      let deps =
            L.nub . L.sort $ concatMap (fmap subModuleName . mkTopLevelDep) allTopLevels
       in ("[TopLevel]", deps)

    depmapAllClasses =
      concatMap
        ( \case
            Left tcl ->
              [ mkTemplateDep tcl,
                mkTHDep tcl
              ]
            Right cls ->
              [ mkRawTypeDep cls,
                mkFFIDep cls,
                mkInterfaceDep cls,
                mkCastDep cls,
                mkImplementationDep cls
              ]
        )
        allclasses
    depmap = topLevelDeps : depmapAllClasses
    allSyms =
      L.nub . L.sort $
        fmap fst depmap ++ concatMap snd depmap
    allISyms :: [(Int, String)]
    allISyms = zip [0 ..] allSyms
    symRevMap = HM.fromList $ fmap swap allISyms
    replace (c, ds) = do
      i <- HM.lookup c symRevMap
      js <- traverse (\d -> HM.lookup d symRevMap) ds
      pure (i, js)
    depmap' = mapMaybe replace depmap

-- | find grouped dependency cycles
findDepCycles :: ([String], [(Int, [Int])]) -> [[String]]
findDepCycles (syms, deps) =
  let symMap = zip [0 ..] syms
      lookupSym i = fromMaybe "<NOTFOUND>" (L.lookup i symMap)
      n = length syms
      bounds = (0, n - 1)
      gr = listArray bounds $ fmap (\i -> fromMaybe [] (L.lookup i deps)) [0 .. n - 1]
      cycleGroups =
        fmap (fmap lookupSym) $ filter (\xs -> length xs > 1) $ fmap flatten (G.scc gr)
   in cycleGroups

-- | locate importing module and imported module in dependency cycles
locateInDepCycles :: (String, String) -> DepCycles -> Maybe (Int, Int)
locateInDepCycles (self, imported) depCycles = do
  cycl <- L.find (self `L.elem`) depCycles
  idxSelf <- self `L.elemIndex` cycl
  idxImported <- imported `L.elemIndex` cycl
  pure (idxSelf, idxImported)
