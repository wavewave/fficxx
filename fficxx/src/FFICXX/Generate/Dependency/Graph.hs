{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FFICXX.Generate.Dependency.Graph where

import Data.Array (listArray)
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tree (flatten)
import Data.Tuple (swap)
import FFICXX.Generate.Dependency
  ( calculateDependency,
    mkTopLevelDep,
  )
import FFICXX.Generate.Name (subModuleName)
import FFICXX.Generate.Type.Class (TopLevel (..))
import FFICXX.Generate.Type.Module
  ( ClassSubmoduleType (..),
    DepCycles,
    TemplateClassSubmoduleType (..),
    UClass,
    UClassSubmodule,
  )

-- TODO: Introduce unique id per submodule.

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
findDepCycles :: ([String], [(Int, [Int])]) -> DepCycles
findDepCycles (syms, deps) =
  let symMap = zip [0 ..] syms
      lookupSym i = fromMaybe "<NOTFOUND>" (L.lookup i symMap)
      n = length syms
      bounds = (0, n - 1)
      gr = listArray bounds $ fmap (\i -> fromMaybe [] (L.lookup i deps)) [0 .. n - 1]
      lookupSymAndRestrictDeps :: [Int] -> [(String, ([String], [String]))]
      lookupSymAndRestrictDeps cycl = fmap go cycl
        where
          go i =
            let sym = lookupSym i
                (rdepsU, rdepsL) =
                  L.partition (< i) $ filter (`elem` cycl) $ fromMaybe [] (L.lookup i deps)
                (rdepsU', rdepsL') = (fmap lookupSym rdepsU, fmap lookupSym rdepsL)
             in (sym, (rdepsU', rdepsL'))
      cycleGroups =
        fmap lookupSymAndRestrictDeps $ filter (\xs -> length xs > 1) $ fmap flatten (G.scc gr)
   in cycleGroups

-- | locate importing module and imported module in dependency cycles
locateInDepCycles :: (String, String) -> DepCycles -> Maybe (Int, Int)
locateInDepCycles (self, imported) depCycles = do
  cycl <- L.find (\xs -> self `L.elem` fmap fst xs) depCycles
  let cyclNoDeps = fmap fst cycl
  idxSelf <- self `L.elemIndex` cyclNoDeps
  idxImported <- imported `L.elemIndex` cyclNoDeps
  pure (idxSelf, idxImported)

gatherHsBootSubmodules :: DepCycles -> [String]
gatherHsBootSubmodules depCycles = do
  cycl <- depCycles
  (_, (_us, ds)) <- cycl
  d <- ds
  pure d
