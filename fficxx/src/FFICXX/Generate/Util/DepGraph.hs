module FFICXX.Generate.Util.DepGraph
  ( drawDepGraph,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import FFICXX.Generate.Dependency
  ( mkModuleDepHighNonSource,
    mkModuleDepHighInplace,
    mkModuleDepRaw,
  )
import FFICXX.Generate.Type.Class
  ( Class (..),
    TemplateClass (..),
  )
import System.FilePath ((<.>))
import System.IO (IOMode (..), hPutStrLn, withFile)
import Text.Dot (Dot, NodeId, attribute, node, showDot, (.->.))

src, box, diamond :: String -> Dot NodeId
src     label = node $ [ ("shape","none"),("label",label) ]
box     label = node $ [ ("shape","box"),("style","rounded"),("label",label) ]
diamond label = node $ [("shape","diamond"),("label",label),("fontsize","10")]

data ClassModuleType
  = CMTRawType
  | CMTInterface
  | CMTImplementation
  | CMTFFI
  | CMTCast

data TemplateClassModuleType
  = TCMTTH
  | TCMTTemplate

-- TODO: Should be used everywhere.
-- | UClass = Unified Class, either template class or ordinary class
type UClass = Either TemplateClass Class

formatOrdinary :: ClassModuleType -> Class -> String
formatOrdinary typ cls =
  class_name cls <.> submod
  where
    submod = case typ of
      CMTRawType -> "RawType"
      CMTInterface -> "Interface"
      CMTImplementation -> "Implementation"
      CMTFFI -> "FFI"
      CMTCast -> "Cast"

formatTemplate :: TemplateClassModuleType -> TemplateClass -> String
formatTemplate typ tcl =
  tclass_name tcl ++ "<T>" <.> submod
  where
    submod = case typ of
      TCMTTH -> "TH"
      TCMTTemplate -> "Template"

-- | Draw dependency graph of modules in graphviz dot format.
drawDepGraph ::
  -- FilePath ->
  [Class] ->
  -- | dot string
  String
drawDepGraph allclasses =
  showDot $ do
    attribute ("size","40,15")
    attribute ("rankdir","LR")
    cs <- traverse box allSyms
    for_ depmap' $ \(i, js) ->
      for_ js $ \j ->
        (cs !! i) .->. (cs !! j)
  where
    mkInterfaceDep :: Class -> (String, [String])
    mkInterfaceDep c =
      let ds = mkModuleDepRaw (Right c)
          format' (Left tcl) = formatTemplate TCMTTemplate tcl
          format' (Right cls) = formatOrdinary CMTRawType cls
       in (formatOrdinary CMTInterface c, fmap format' ds)
    depmap = fmap mkInterfaceDep allclasses
    allSyms =
      L.nub . L.sort $
        fmap fst depmap ++ concatMap snd depmap
    allISyms :: [(Int, String)]
    allISyms = zip [0..] allSyms
    symMap = HM.fromList allISyms
    symRevMap = HM.fromList $ fmap swap allISyms
    replace (c, ds) = do
      i <- HM.lookup c symRevMap
      js <- traverse (\d -> HM.lookup d symRevMap) ds
      pure (i, js)
    depmap' = mapMaybe replace depmap
