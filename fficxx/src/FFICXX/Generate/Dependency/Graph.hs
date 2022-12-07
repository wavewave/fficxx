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
    UClassSubmodule,
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
constructDepGraph allClasses allTopLevels = (allSyms, depmap')
  where
    -- for classes/template classes
    mkDep :: UClass -> [(UClassSubmodule, [UClassSubmodule])]
    mkDep c =
      case c of
        Left tcl ->
          fmap
            (build . Left . (,tcl))
            [TCSTTemplate, TCSTTH]
        Right cls ->
          fmap
            (build . Right . (,cls))
            [CSTRawType, CSTFFI, CSTInterface, CSTCast, CSTImplementation]
      where
        build x = (x, calculateDependency x)

    dep2Name :: [(UClassSubmodule, [UClassSubmodule])] -> [(String, [String])]
    dep2Name = fmap (\(x, ys) -> (subModuleName x, fmap subModuleName ys))
    -- TopLevel
    topLevelDeps :: (String, [String])
    topLevelDeps =
      let deps =
            L.nub . L.sort $ concatMap (fmap subModuleName . mkTopLevelDep) allTopLevels
       in ("[TopLevel]", deps)

    depmapAllClasses = concatMap (dep2Name . mkDep) allClasses
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
