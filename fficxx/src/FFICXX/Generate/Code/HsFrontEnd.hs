{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsFrontEnd where

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

--------------------------

------------------------
-- Top Level Function --
------------------------

genTopLevelDef :: TLOrdinary -> [Decl ()]
genTopLevelDef f@TopLevelFunction {..} =
  let fname = hsFrontNameForTopLevel (TLOrdinary f)
      HsFunSig typs assts =
        extractArgRetTypes
          Nothing
          False
          (CFunSig toplevelfunc_args toplevelfunc_ret)
      sig = tyForall Nothing (Just (cxTuple assts)) (foldr1 tyfun typs)
      xformerstr = let len = length toplevelfunc_args in if len > 0 then "xform" <> show (len - 1) else "xformnull"
      cfname = "c_" <> toLowers fname
      rhs = app (mkVar xformerstr) (mkVar cfname)
   in mkFun fname sig [] rhs Nothing
genTopLevelDef v@TopLevelVariable {..} =
  let fname = hsFrontNameForTopLevel (TLOrdinary v)
      cfname = "c_" <> toLowers fname
      rtyp = convertCpp2HS Nothing toplevelvar_ret
      sig = tyapp (tycon "IO") rtyp
      rhs = app (mkVar "xformnull") (mkVar cfname)
   in mkFun fname sig [] rhs Nothing

------------
-- Export --
------------

genExport :: Class -> [ExportSpec ()]
genExport c =
  let espec n =
        if null . (filter isVirtualFunc) $ (class_funcs c)
          then eabs nonamespace (unqual n)
          else ethingall (unqual n)
   in if isAbstractClass c
        then [espec (typeclassName c)]
        else
          [ ethingall (unqual ((fst . hsClassName) c)),
            espec (typeclassName c),
            evar (unqual ("upcast" <> (fst . hsClassName) c)),
            evar (unqual ("downcast" <> (fst . hsClassName) c))
          ]
            <> genExportConstructorAndNonvirtual c
            <> genExportStatic c

-- | constructor and non-virtual function
genExportConstructorAndNonvirtual :: Class -> [ExportSpec ()]
genExportConstructorAndNonvirtual c = map (evar . unqual) fns
  where
    fs = class_funcs c
    fns =
      map
        (aliasedFuncName c)
        ( constructorFuncs fs
            <> nonVirtualNotNewFuncs fs
        )

-- | staic function export list
genExportStatic :: Class -> [ExportSpec ()]
genExportStatic c = map (evar . unqual) fns
  where
    fs = class_funcs c
    fns = map (aliasedFuncName c) (staticFuncs fs)

------------
-- Import --
------------

-- TODO: Remove
genExtraImport_ :: ClassModule -> [ImportDecl ()]
genExtraImport_ cm = map mkImport (cmExtraImport cm)

-- This is the new version.
genExtraImport :: ClassModule -> [Ex.ImportDecl Ex.GhcPs]
genExtraImport cm = fmap Ex.mkImport (cmExtraImport cm)

genImportInModule :: Class -> [ImportDecl ()]
genImportInModule x = map (\y -> mkImport (getClassModuleBase x <.> y)) ["RawType", "Interface", "Implementation"]

-- OLD
-- TODO: Remove
genImportInCast_ :: ClassModule -> [ImportDecl ()]
genImportInCast_ m =
  fmap (mkImport . subModuleName) $ cmImportedSubmodulesForCast m

-- NEW

-- | generate import list for a given top-level ordinary function
--   currently this may generate duplicate import list.
-- TODO: eliminate duplicated imports.
-- TODO2: should be refactored out.
genImportForTLOrdinary :: TLOrdinary -> [ImportDecl ()]
genImportForTLOrdinary f =
  let dep4func = extractClassDepForTLOrdinary f
      ecs = returnDependency dep4func ++ argumentDependency dep4func
      cmods = L.nub $ map getClassModuleBase $ rights ecs
      tmods = L.nub $ map getTClassModuleBase $ lefts ecs
   in concatMap (\x -> map (\y -> mkImport (x <.> y)) ["RawType", "Cast", "Interface"]) cmods
        <> concatMap (\x -> map (\y -> mkImport (x <.> y)) ["Template"]) tmods

-- | generate import list for a given top-level template function
--   currently this may generate duplicate import list.
-- TODO: eliminate duplicated imports.
-- TODO2: should be refactored out.
genImportForTLTemplate :: TLTemplate -> [ImportDecl ()]
genImportForTLTemplate f =
  let dep4func = extractClassDepForTLTemplate f
      ecs = returnDependency dep4func ++ argumentDependency dep4func
      cmods = L.nub $ map getClassModuleBase $ rights ecs
      tmods = L.nub $ map getTClassModuleBase $ lefts ecs
   in concatMap (\x -> map (\y -> mkImport (x <.> y)) ["RawType", "Cast", "Interface"]) cmods
        <> concatMap (\x -> map (\y -> mkImport (x <.> y)) ["Template"]) tmods

-- | generate import list for top level module
genImportInTopLevel ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  [ImportDecl ()]
genImportInTopLevel modname (mods, _tmods) =
  map (mkImport . cmModule) mods
    ++ map mkImport [modname <.> "Template", modname <.> "TH", modname <.> "Ordinary"]
