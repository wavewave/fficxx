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
import FFICXX.Generate.Dependency.Graph
  ( UClass,
    constructDepGraph,
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
    (allSyms, depmap') = constructDepGraph allclasses allTopLevels
