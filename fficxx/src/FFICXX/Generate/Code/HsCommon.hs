module FFICXX.Generate.Code.HsCommon
  ( genExtraImport,
  )
where

import FFICXX.Generate.Type.Module (ClassModule (..))
import FFICXX.Generate.Util.GHCExactPrint (mkImport)
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax (ImportDecl)

genExtraImport :: ClassModule -> [ImportDecl GhcPs]
genExtraImport cm = fmap mkImport (cmExtraImport cm)
