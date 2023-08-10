{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsCommon
  ( genExtraImport_,
    genExtraImport,
    genImportInCast_,
  )
where

import FFICXX.Generate.Name (subModuleName)
import FFICXX.Generate.Type.Module (ClassModule (..))
import qualified FFICXX.Generate.Util.GHCExactPrint as Ex
import FFICXX.Generate.Util.HaskellSrcExts (mkImport)
import qualified GHC.Hs as Ex
import Language.Haskell.Exts.Syntax (ImportDecl)

-- TODO: Remove
genExtraImport_ :: ClassModule -> [ImportDecl ()]
genExtraImport_ cm = map mkImport (cmExtraImport cm)

-- This is the new version.
genExtraImport :: ClassModule -> [Ex.ImportDecl Ex.GhcPs]
genExtraImport cm = fmap Ex.mkImport (cmExtraImport cm)

-- OLD
-- TODO: Remove
genImportInCast_ :: ClassModule -> [ImportDecl ()]
genImportInCast_ m =
  fmap (mkImport . subModuleName) $ cmImportedSubmodulesForCast m
