{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsTemplate
  ( genImportInTemplate,
    genTmplInterface,
  )
where

import FFICXX.Generate.Code.HsCast (castBody_)
import FFICXX.Generate.Code.Primitive
  ( functionSignatureT,
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
import FFICXX.Generate.Util.HaskellSrcExts
  ( clsDecl,
    con,
    conDecl,
    cxEmpty,
    insDecl,
    insType,
    mkBind1,
    mkClass,
    mkData,
    mkFunSig,
    mkImport,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkTBind,
    mkTVar,
    mkVar,
    qualConDecl,
    tyPtr,
    tyapp,
    tycon,
  )
import Language.Haskell.Exts.Build
  ( name,
    pApp,
  )
import Language.Haskell.Exts.Syntax
  ( Decl,
    ImportDecl,
  )

genImportInTemplate :: TemplateClass -> [ImportDecl ()]
genImportInTemplate t0 =
  fmap (mkImport . subModuleName) $ calculateDependency $ Left (TCSTTemplate, t0)

genTmplInterface :: TemplateClass -> [Decl ()]
genTmplInterface t =
  [ mkData rname (map mkTBind tps) [] Nothing,
    mkNewtype
      hname
      (map mkTBind tps)
      [qualConDecl Nothing Nothing (conDecl hname [tyapp tyPtr rawtype])]
      Nothing,
    mkClass cxEmpty (typeclassNameT t) (map mkTBind tps) methods,
    mkInstance cxEmpty "FPtr" [hightype] fptrbody,
    mkInstance cxEmpty "Castable" [hightype, tyapp tyPtr rawtype] castBody_
  ]
  where
    (hname, rname) = hsTemplateClassName t
    tps = tclass_params t
    fs = tclass_funcs t
    vfs = tclass_vars t
    rawtype = foldl1 tyapp (tycon rname : map mkTVar tps)
    hightype = foldl1 tyapp (tycon hname : map mkTVar tps)
    sigdecl f = mkFunSig (hsTmplFuncName t f) (functionSignatureT t f)
    sigdeclV vf =
      let f_g = tmplAccessorToTFun vf Getter
          f_s = tmplAccessorToTFun vf Setter
       in [sigdecl f_g, sigdecl f_s]
    methods = map (clsDecl . sigdecl) fs ++ (map clsDecl . concatMap sigdeclV) vfs
    fptrbody =
      [ insType (tyapp (tycon "Raw") hightype) rawtype,
        insDecl (mkBind1 "get_fptr" [pApp (name hname) [mkPVar "ptr"]] (mkVar "ptr") Nothing),
        insDecl (mkBind1 "cast_fptr_to_obj" [] (con hname) Nothing)
      ]
