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

import Data.Either (lefts, rights)
import qualified Data.List as L
import FFICXX.Generate.Code.Primitive
  ( CFunSig (..),
    HsFunSig (..),
    convertCpp2HS,
    extractArgRetTypes,
  )
import FFICXX.Generate.Dependency
  ( argumentDependency,
    extractClassDepForTLOrdinary,
    extractClassDepForTLTemplate,
    returnDependency,
  )
import FFICXX.Generate.Name
  ( aliasedFuncName,
    getClassModuleBase,
    getTClassModuleBase,
    hsClassName,
    hsFrontNameForTopLevel,
    subModuleName,
    typeclassName,
  )
import FFICXX.Generate.Type.Class
  ( Class (..),
    TLOrdinary (..),
    TLTemplate,
    TopLevel (TLOrdinary),
    constructorFuncs,
    isAbstractClass,
    isVirtualFunc,
    nonVirtualNotNewFuncs,
    staticFuncs,
  )
import FFICXX.Generate.Type.Module
  ( ClassModule (..),
    TemplateClassModule (..),
  )
import FFICXX.Generate.Util (toLowers)
--
import qualified FFICXX.Generate.Util.GHCExactPrint as Ex
import FFICXX.Generate.Util.HaskellSrcExts
  ( cxTuple,
    eabs,
    ethingall,
    evar,
    mkFun,
    mkImport,
    mkVar,
    nonamespace,
    tyForall,
    tyapp,
    tycon,
    tyfun,
    unqual,
  )
import qualified GHC.Hs as Ex
import Language.Haskell.Exts.Build (app)
import Language.Haskell.Exts.Syntax
  ( Decl,
    ExportSpec,
    ImportDecl,
  )
import System.FilePath ((<.>))

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
