{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsTemplate
  ( genImportInTemplate,
    genTmplInterface,
  )
where

import FFICXX.Generate.Code.HsCast (castBody)
import FFICXX.Generate.Code.Primitive
  ( functionSignatureT',
    tmplAccessorToTFun,
  )
import FFICXX.Generate.Dependency (calculateDependency)
import FFICXX.Generate.Name
  ( hsTemplateClassName,
    hsTmplFuncName,
    subModuleName,
    typeclassNameT,
  )
import FFICXX.Generate.Type.Class
  ( Accessor (Getter, Setter),
    TemplateClass (..),
  )
import FFICXX.Generate.Type.Module
  ( TemplateClassSubmoduleType (..),
  )
import FFICXX.Generate.Util.GHCExactPrint
  ( con,
    conDecl,
    cxEmpty,
    instD,
    mkBind1_,
    mkClass,
    mkData,
    mkFunSig,
    mkImport,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkTBind,
    mkTVar,
    mkTypeFamInst,
    mkVar,
    pApp,
    parP,
    tyParen,
    tyPtr,
    tyapp,
    tycon,
  )
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax
  ( HsDecl (TyClD),
    ImportDecl,
    noExtField,
  )

genImportInTemplate :: TemplateClass -> [ImportDecl GhcPs]
genImportInTemplate t0 =
  fmap (mkImport . subModuleName) $ calculateDependency $ Left (TCSTTemplate, t0)

genTmplInterface :: TemplateClass -> [HsDecl GhcPs]
genTmplInterface t =
  [ TyClD noExtField (mkData rname (fmap mkTBind tps) [] []),
    TyClD noExtField $
      mkNewtype
        hname
        (fmap mkTBind tps)
        (conDecl hname [tyParen (tyapp tyPtr (tyParen rawtype))])
        [],
    TyClD noExtField $
      mkClass cxEmpty (typeclassNameT t) (fmap mkTBind tps) methods,
    instD $
      mkInstance cxEmpty "FPtr" [hightype] fptrfam fptrbody,
    instD $
      mkInstance cxEmpty "Castable" [hightype, tyapp tyPtr (tyParen rawtype)] [] castBody
  ]
  where
    (hname, rname) = hsTemplateClassName t
    tps = tclass_params t
    fs = tclass_funcs t
    vfs = tclass_vars t
    rawtype = foldl1 tyapp (tycon rname : map mkTVar tps)
    hightype = foldl1 tyapp (tycon hname : map mkTVar tps)
    sigdecl f = mkFunSig (hsTmplFuncName t f) (functionSignatureT' t f)
    sigdeclV vf =
      let f_g = tmplAccessorToTFun vf Getter
          f_s = tmplAccessorToTFun vf Setter
       in [sigdecl f_g, sigdecl f_s]
    methods = fmap sigdecl fs ++ concatMap sigdeclV vfs
    fptrfam = [mkTypeFamInst "Raw" [tyParen hightype] rawtype]
    fptrbody =
      [ mkBind1_
          "get_fptr"
          [parP (pApp hname [mkPVar "ptr"])]
          (mkVar "ptr"),
        mkBind1_
          "cast_fptr_to_obj"
          []
          (con hname)
      ]
