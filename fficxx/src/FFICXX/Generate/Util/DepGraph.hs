module FFICXX.Generate.Util.DepGraph
  ( drawDepGraph,
  )
where

import Data.Foldable (for_)
import FFICXX.Generate.Dependency.Graph
  ( constructDepGraph,
  )
import FFICXX.Generate.Type.Class (TopLevel (..))
import FFICXX.Generate.Type.Module (UClass)
import Text.Dot (Dot, NodeId, attribute, node, showDot, (.->.))

box :: String -> Dot NodeId
box label = node $ [("shape", "box"), ("style", "rounded"), ("label", label)]

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
