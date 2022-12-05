{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module FFICXX.Generate.Util.DepGraph
  ( drawDepGraph,
  )
where

import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
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
  ( ClassModuleType (..),
    TemplateClassModuleType (..),
    hsClassName,
    hsTemplateClassName,
    subModuleName,
  )
import FFICXX.Generate.Type.Class
  ( Class (..),
    TemplateClass (..),
    TopLevel (..),
  )
import System.FilePath ((<.>))
import System.IO (IOMode (..), hPutStrLn, withFile)
import Text.Dot (Dot, NodeId, attribute, node, showDot, (.->.))

src, box, diamond :: String -> Dot NodeId
src label = node $ [("shape", "none"), ("label", label)]
box label = node $ [("shape", "box"), ("style", "rounded"), ("label", label)]
diamond label = node $ [("shape", "diamond"), ("label", label), ("fontsize", "10")]

-- TODO: Should be used everywhere.

-- | UClass = Unified Class, either template class or ordinary class
type UClass = Either TemplateClass Class

-- | Draw dependency graph of modules in graphviz dot format.
drawDepGraph ::
  -- | list of all classes, either template class or ordinary class.
  [UClass] ->
  -- | list of all top-level functions.
  [TopLevel] ->
  -- | dot string
  String
drawDepGraph allclasses allTopLevels =
  showDot $ do
    attribute ("size", "40,15")
    attribute ("rankdir", "LR")
    cs <- traverse box allSyms
    for_ depmap' $ \(i, js) ->
      for_ js $ \j ->
        (cs !! i) .->. (cs !! j)
  where
    -- RawType dependency is trivial
    mkRawTypeDep :: Class -> (String, [String])
    mkRawTypeDep c =
      let rawtype = subModuleName (Right (CMTRawType, c))
       in (rawtype, [])
    -- FFI
    mkFFIDep :: Class -> (String, [String])
    mkFFIDep c =
      let ffi = subModuleName (Right (CMTFFI, c))
          depRawSelf = subModuleName (Right (CMTRawType, c))
          depsFFI =
            let ds = mkModuleDepFFI (Right c)
                format' (Left tcl) = subModuleName $ Left (TCMTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CMTRawType, cls)
             in fmap format' ds
       in (ffi, [depRawSelf] ++ depsFFI)
    mkInterfaceDep :: Class -> (String, [String])
    mkInterfaceDep c =
      let interface = subModuleName $ Right (CMTInterface, c)
          depRawSelf = subModuleName $ Right (CMTRawType, c)
          depsRaw =
            let ds = mkModuleDepRaw (Right c)
                format' (Left tcl) = subModuleName $ Left (TCMTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CMTRawType, cls)
             in fmap format' ds
          depsExt =
            let ds = mkModuleDepExternal (Right c)
                format' (Left tcl) = subModuleName $ Left (TCMTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CMTInterface, cls)
             in fmap format' ds
          depsInplace =
            let ds = mkModuleDepInplace (Right c)
                format' (Left tcl) = subModuleName $ Left (TCMTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CMTInterface, cls)
             in fmap format' ds
       in (interface, [depRawSelf] ++ depsRaw ++ depsExt ++ depsInplace)
    -- Cast
    mkCastDep :: Class -> (String, [String])
    mkCastDep c =
      let cast = subModuleName $ Right (CMTCast, c)
          depsSelf = fmap (subModuleName . Right . (,c)) [CMTRawType, CMTInterface]
       in (cast, depsSelf)
    -- Implementation
    -- TODO: THIS IS INVOLVED! NEED TO REFACTOR THINGS OUT.
    mkImplementationDep :: Class -> (String, [String])
    mkImplementationDep c =
      let implementation = subModuleName $ Right (CMTImplementation, c)
          depsSelf =
            fmap (subModuleName . Right . (,c)) [CMTRawType, CMTFFI, CMTInterface, CMTCast]
          deps =
            let dsFFI = mkModuleDepFFI (Right c)
                dsParents = L.nub $ map Right $ class_allparents c
                dsNonParents = filter (not . (flip elem dsParents)) dsFFI
                format (Left tcl) = [subModuleName $ Left (TCMTTemplate, tcl)]
                format (Right cls) =
                  fmap (subModuleName . Right . (,cls)) [CMTRawType, CMTCast, CMTInterface]
             in concatMap format (dsNonParents ++ dsParents)
       in (implementation, depsSelf ++ deps)
    -- Template Class part
    -- <TClass>.Template
    mkTemplateDep :: TemplateClass -> (String, [String])
    mkTemplateDep t =
      let template = subModuleName $ Left (TCMTTemplate, t)
          depsRaw =
            let ds = mkModuleDepRaw (Left t)
                format' (Left tcl) = subModuleName $ Left (TCMTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CMTRawType, cls)
             in fmap format' ds
          depsInplace =
            let ds = mkModuleDepInplace (Left t)
                format' (Left tcl) = subModuleName $ Left (TCMTTemplate, tcl)
                format' (Right cls) = subModuleName $ Right (CMTInterface, cls)
             in fmap format' ds
       in (template, depsRaw ++ depsInplace)
    -- <TClass>.TH
    mkTHDep :: TemplateClass -> (String, [String])
    mkTHDep t =
      let th = subModuleName $ Left (TCMTTH, t)
          deps =
            let dsRaw = mkModuleDepRaw (Left t)
                dsInplace = mkModuleDepInplace (Left t)

                format' (Left tcl) = [subModuleName $ Left (TCMTTemplate, tcl)]
                format' (Right cls) =
                  fmap (subModuleName . Right . (,cls)) [CMTRawType, CMTCast, CMTInterface]
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
