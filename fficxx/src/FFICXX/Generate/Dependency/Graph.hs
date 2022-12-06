{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FFICXX.Generate.Dependency.Graph where

import Data.Array (listArray)
import Data.Foldable (for_)
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tree (drawForest, flatten)
import Data.Tuple (swap)
import FFICXX.Generate.Dependency
  ( class_allparents,
    mkModuleDepExternal,
    mkModuleDepFFI,
    mkModuleDepInplace,
    mkModuleDepRaw,
    mkTopLevelDep,
  )
import FFICXX.Generate.Name
  ( hsClassName,
    hsTemplateClassName,
    subModuleName,
  )
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
import System.FilePath ((<.>))
import System.IO (IOMode (..), hPutStrLn, withFile)

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
          depsFFI =
            let ds = mkModuleDepFFI (Right c)
                format' (Left tcl) = subModuleName $ Left (TCSTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CSTRawType, cls)
             in fmap format' ds
       in (ffi, [depRawSelf] ++ depsFFI)
    mkInterfaceDep :: Class -> (String, [String])
    mkInterfaceDep c =
      let interface = subModuleName $ Right (CSTInterface, c)
          depRawSelf = subModuleName $ Right (CSTRawType, c)
          depsRaw =
            let ds = mkModuleDepRaw (Right c)
                format' (Left tcl) = subModuleName $ Left (TCSTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CSTRawType, cls)
             in fmap format' ds
          depsExt =
            let ds = mkModuleDepExternal (Right c)
                format' (Left tcl) = subModuleName $ Left (TCSTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CSTInterface, cls)
             in fmap format' ds
          depsInplace =
            let ds = mkModuleDepInplace (Right c)
                format' (Left tcl) = subModuleName $ Left (TCSTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CSTInterface, cls)
             in fmap format' ds
       in (interface, [depRawSelf] ++ depsRaw ++ depsExt ++ depsInplace)
    -- Cast
    mkCastDep :: Class -> (String, [String])
    mkCastDep c =
      let cast = subModuleName $ Right (CSTCast, c)
          depsSelf = fmap (subModuleName . Right . (,c)) [CSTRawType, CSTInterface]
       in (cast, depsSelf)
    -- Implementation
    -- TODO: THIS IS INVOLVED! NEED TO REFACTOR THINGS OUT.
    mkImplementationDep :: Class -> (String, [String])
    mkImplementationDep c =
      let implementation = subModuleName $ Right (CSTImplementation, c)
          depsSelf =
            fmap (subModuleName . Right . (,c)) [CSTRawType, CSTFFI, CSTInterface, CSTCast]
          deps =
            let dsFFI = mkModuleDepFFI (Right c)
                dsParents = L.nub $ map Right $ class_allparents c
                dsNonParents = filter (not . (flip elem dsParents)) dsFFI
                format (Left tcl) = [subModuleName $ Left (TCSTTemplate, tcl)]
                format (Right cls) =
                  fmap (subModuleName . Right . (,cls)) [CSTRawType, CSTCast, CSTInterface]
             in concatMap format (dsNonParents ++ dsParents)
       in (implementation, depsSelf ++ deps)
    -- Template Class part
    -- <TClass>.Template
    mkTemplateDep :: TemplateClass -> (String, [String])
    mkTemplateDep t =
      let template = subModuleName $ Left (TCSTTemplate, t)
          depsRaw =
            let ds = mkModuleDepRaw (Left t)
                format' (Left tcl) = subModuleName $ Left (TCSTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CSTRawType, cls)
             in fmap format' ds
          depsInplace =
            let ds = mkModuleDepInplace (Left t)
                format' (Left tcl) = subModuleName $ Left (TCSTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CSTInterface, cls)
             in fmap format' ds
       in (template, depsRaw ++ depsInplace)
    -- <TClass>.TH
    mkTHDep :: TemplateClass -> (String, [String])
    mkTHDep t =
      let th = subModuleName $ Left (TCSTTH, t)
          deps =
            let dsRaw = mkModuleDepRaw (Left t)
                dsInplace = mkModuleDepInplace (Left t)

                format' (Left tcl) = [subModuleName $ Left (TCSTTemplate, tcl)]
                format' (Right cls) =
                  fmap (subModuleName . Right . (,cls)) [CSTRawType, CSTCast, CSTInterface]
             in concatMap format' (dsRaw ++ dsInplace)
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
            Left tcl -> [mkTemplateDep tcl]
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
    symMap = HM.fromList allISyms
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
