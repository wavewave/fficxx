{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsFrontEnd where

import Control.Monad.Reader (Reader)
import Data.Either (lefts, rights)
import qualified Data.List as L
import FFICXX.Generate.Code.Primitive
  ( CFunSig (..),
    HsFunSig (..),
    accessorSignature,
    classConstraints,
    convertCpp2HS,
    extractArgRetTypes,
    functionSignature,
    hsFuncXformer,
  )
import FFICXX.Generate.Dependency
  ( argumentDependency,
    extractClassDepForTLOrdinary,
    extractClassDepForTLTemplate,
    returnDependency,
  )
import FFICXX.Generate.Dependency.Graph
  ( getCyclicDepSubmodules,
    locateInDepCycles,
  )
import FFICXX.Generate.Name
  ( accessorName,
    aliasedFuncName,
    getClassModuleBase,
    getTClassModuleBase,
    hsClassName,
    hsFrontNameForTopLevel,
    hsFuncName,
    hscAccessorName,
    hscFuncName,
    subModuleName,
    typeclassName,
  )
import FFICXX.Generate.Type.Annotate (AnnotateMap)
import FFICXX.Generate.Type.Class
  ( Accessor (..),
    Class (..),
    TLOrdinary (..),
    TLTemplate,
    TopLevel (TLOrdinary),
    Types (..),
    constructorFuncs,
    isAbstractClass,
    isNewFunc,
    isVirtualFunc,
    nonVirtualNotNewFuncs,
    staticFuncs,
    virtualFuncs,
  )
import FFICXX.Generate.Type.Module
  ( ClassModule (..),
    DepCycles,
    TemplateClassModule (..),
  )
import FFICXX.Generate.Util (toLowers)
--
import qualified FFICXX.Generate.Util.GHCExactPrint as Ex
import FFICXX.Generate.Util.HaskellSrcExts
  ( classA,
    clsDecl,
    con,
    conDecl,
    cxEmpty,
    cxTuple,
    eabs,
    ethingall,
    evar,
    ihcon,
    insDecl,
    insType,
    irule,
    mkBind1,
    mkClass,
    mkData,
    mkDeriving,
    mkFun,
    mkFunSig,
    mkImport,
    mkImportSrc,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkPVarSig,
    mkTBind,
    mkTVar,
    mkVar,
    nonamespace,
    pbind,
    qualConDecl,
    tyForall,
    tyPtr,
    tyapp,
    tycon,
    tyfun,
    unkindedVar,
    unqual,
  )
import qualified GHC.Hs as Ex
import Language.Haskell.Exts.Build (app, letE, name, pApp)
import Language.Haskell.Exts.Syntax
  ( Decl,
    ExportSpec,
    ImportDecl,
  )
import qualified Language.Haskell.Syntax as Ex
import System.FilePath ((<.>))

-------------------

genHsFrontInst :: Class -> Class -> [Decl ()]
genHsFrontInst parent child
  | (not . isAbstractClass) child =
      let idecl = mkInstance cxEmpty (typeclassName parent) [convertCpp2HS (Just child) SelfType] body
          defn f = mkBind1 (hsFuncName child f) [] rhs Nothing
            where
              rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName child f))
          body = map (insDecl . defn) . virtualFuncs . class_funcs $ parent
       in [idecl]
  | otherwise = []

---------------------

genHsFrontInstNew ::
  -- | only concrete class
  Class ->
  Reader AnnotateMap [Decl ()]
genHsFrontInstNew c = do
  -- amap <- ask
  let fs = filter isNewFunc (class_funcs c)
  return . flip concatMap fs $ \f ->
    let -- for the time being, let's ignore annotation.
        -- cann = maybe "" id $ M.lookup (PkgMethod, constructorName c) amap
        -- newfuncann = mkComment 0 cann
        rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

genHsFrontInstNonVirtual :: Class -> [Decl ()]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing
  where
    nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

-----

genHsFrontInstStatic :: Class -> [Decl ()]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

-----

genHsFrontInstVariables :: Class -> [Decl ()]
genHsFrontInstVariables c =
  flip concatMap (class_vars c) $ \v ->
    let rhs accessor =
          app
            (mkVar (case accessor of Getter -> "xform0"; _ -> "xform1"))
            (mkVar (hscAccessorName c v accessor))
     in mkFun (accessorName c v Getter) (accessorSignature c v Getter) [] (rhs Getter) Nothing
          <> mkFun (accessorName c v Setter) (accessorSignature c v Setter) [] (rhs Setter) Nothing

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
genImportInCast :: ClassModule -> [Ex.ImportDecl Ex.GhcPs]
genImportInCast m =
  fmap (Ex.mkImport . subModuleName) $ cmImportedSubmodulesForCast m

genImportInImplementation :: ClassModule -> [ImportDecl ()]
genImportInImplementation m =
  fmap (mkImport . subModuleName) $ cmImportedSubmodulesForImplementation m

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
