module FFICXX.Generate.Util.DepGraph
  ( drawDepGraph,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Foldable (for_)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import FFICXX.Generate.Dependency (mkModuleDepHighNonSource, mkModuleDepHighSource)
import FFICXX.Generate.Type.Class
  ( Class (..),
    TemplateClass (..),
  )
import System.IO (IOMode (..), hPutStrLn, withFile)
import Text.Dot (Dot, NodeId, attribute, node, showDot, (.->.))

src, box, diamond :: String -> Dot NodeId
src     label = node $ [ ("shape","none"),("label",label) ]
box     label = node $ [ ("shape","box"),("style","rounded"),("label",label) ]
diamond label = node $ [("shape","diamond"),("label",label),("fontsize","10")]

drawDepGraph :: FilePath -> [Either TemplateClass Class] -> IO ()
drawDepGraph fp allclasses = do
  withFile fp WriteMode $ \h ->
    hPutStrLn h $ showDot $ do
      attribute ("size","40,15")
      attribute ("rankdir","LR")
      cs <- traverse box allSyms
      for_ depmap' $ \(i, js) ->
        for_ js $ \j ->
          (cs !! i) .->. (cs !! j)
  where
    format :: Either TemplateClass Class -> String
    format (Left t) = tclass_name t ++ "<T>"
    format (Right c) = class_name c
    mkDep :: Either TemplateClass Class -> (String, [String])
    mkDep c =
      let ds = mkModuleDepHighNonSource c <> mkModuleDepHighSource c
       in (format c, fmap format ds)
    depmap = fmap mkDep allclasses
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
