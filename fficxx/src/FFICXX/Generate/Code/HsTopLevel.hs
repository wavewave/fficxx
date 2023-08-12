{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsTopLevel
  ( -- * exports
    genExport,

    -- * imports
    genImportInModule,
    genImportInTopLevel,

    -- * top-level decls and defs
    genTopLevelDef,
    genImportForTLOrdinary,
    genImportForTLTemplate,

    -- * toplevel template
    genTLTemplateInterface,
    genTLTemplateImplementation,
    genTLTemplateInstance,
  )
where

import Data.Either (lefts, rights)
import qualified Data.List as L
import FFICXX.Generate.Code.Cpp
  ( genTLTmplFunCpp,
  )
import FFICXX.Generate.Code.Primitive
  ( CFunSig (..),
    HsFunSig (..),
    cxx2HsType,
    cxx2HsType4Tmpl,
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
    typeclassName,
  )
import FFICXX.Generate.Type.Class
  ( Arg (..),
    Class (..),
    TLOrdinary (..),
    TLTemplate (..),
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
    TopLevelImportHeader (..),
  )
import FFICXX.Generate.Util (firstUpper, toLowers)
--
import FFICXX.Generate.Util.GHCExactPrint
  ( app,
    bracketExp,
    caseE,
    con,
    cxEmpty,
    cxTuple,
    doE,
    eabs,
    emodule,
    ethingall,
    evar,
    inapp,
    lamE,
    letE,
    listE,
    mkBind1,
    mkBind1_,
    mkBindStmt,
    mkBodyStmt,
    mkClass,
    mkFun,
    mkFunSig,
    mkFun_,
    mkImport,
    mkLetStmt,
    mkPVar,
    mkTBind,
    mkVar,
    op,
    pTuple,
    par,
    parenSplice,
    pbind_,
    qualTy,
    strE,
    toLocalBinds,
    tupleE,
    tyForall,
    tyParen,
    tySplice,
    tyTupleBoxed,
    tyapp,
    tycon,
    tyfun,
    tylist,
    typeBracket,
    unqual,
    valBinds,
    wildcard,
  )
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import FFICXX.Runtime.TH (IsCPrimitive (CPrim, NonCPrim))
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax
  ( HsDecl (TyClD),
    IE,
    ImportDecl,
    noExtField,
  )
import System.FilePath ((<.>))

--
-- exports
--

------------
-- Export --
------------

genExport :: Class -> [IE GhcPs]
genExport c =
  let espec n =
        if null . (filter isVirtualFunc) $ (class_funcs c)
          then eabs n
          else ethingall n
   in if isAbstractClass c
        then [espec (typeclassName c)]
        else
          [ ethingall ((fst . hsClassName) c),
            espec (typeclassName c),
            evar ("upcast" <> (fst . hsClassName) c),
            evar ("downcast" <> (fst . hsClassName) c)
          ]
            <> genExportConstructorAndNonvirtual c
            <> genExportStatic c

-- | constructor and non-virtual function
genExportConstructorAndNonvirtual :: Class -> [IE GhcPs]
genExportConstructorAndNonvirtual c = fmap evar fns
  where
    fs = class_funcs c
    fns =
      map
        (aliasedFuncName c)
        ( constructorFuncs fs
            <> nonVirtualNotNewFuncs fs
        )

-- | staic function export list
genExportStatic :: Class -> [IE GhcPs]
genExportStatic c = fmap evar fns
  where
    fs = class_funcs c
    fns = map (aliasedFuncName c) (staticFuncs fs)

--
-- imports
--

-- | module summary re-exports
genImportInModule :: Class -> [ImportDecl GhcPs]
genImportInModule x = map (\y -> mkImport (getClassModuleBase x <.> y)) ["RawType", "Interface", "Implementation"]

-- | top=level
genImportInTopLevel ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  [ImportDecl GhcPs]
genImportInTopLevel modname (mods, _tmods) =
  map (mkImport . cmModule) mods
    ++ map mkImport [modname <.> "Template", modname <.> "TH", modname <.> "Ordinary"]

--
-- declarations and definitions
--

genTopLevelDef :: TLOrdinary -> [HsDecl GhcPs]
genTopLevelDef f@TopLevelFunction {..} =
  let fname = hsFrontNameForTopLevel (TLOrdinary f)
      HsFunSig typs assts =
        extractArgRetTypes
          Nothing
          False
          (CFunSig toplevelfunc_args toplevelfunc_ret)
      sig = qualTy (cxTuple assts) (foldr1 tyfun typs)
      xformerstr = let len = length toplevelfunc_args in if len > 0 then "xform" <> show (len - 1) else "xformnull"
      cfname = "c_" <> toLowers fname
      rhs = app (mkVar xformerstr) (mkVar cfname)
   in mkFun_ fname sig [] rhs
genTopLevelDef v@TopLevelVariable {..} =
  let fname = hsFrontNameForTopLevel (TLOrdinary v)
      cfname = "c_" <> toLowers fname
      rtyp = cxx2HsType Nothing toplevelvar_ret
      sig = tyapp (tycon "IO") (tyParen rtyp)
      rhs = app (mkVar "xformnull") (mkVar cfname)
   in mkFun_ fname sig [] rhs

-- | generate import list for a given top-level ordinary function
--   currently this may generate duplicate import list.
-- TODO: eliminate duplicated imports.
-- TODO2: should be refactored out.
genImportForTLOrdinary :: TLOrdinary -> [ImportDecl GhcPs]
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
genImportForTLTemplate :: TLTemplate -> [ImportDecl GhcPs]
genImportForTLTemplate f =
  let dep4func = extractClassDepForTLTemplate f
      ecs = returnDependency dep4func ++ argumentDependency dep4func
      cmods = L.nub $ map getClassModuleBase $ rights ecs
      tmods = L.nub $ map getTClassModuleBase $ lefts ecs
   in concatMap (\x -> map (\y -> mkImport (x <.> y)) ["RawType", "Cast", "Interface"]) cmods
        <> concatMap (\x -> map (\y -> mkImport (x <.> y)) ["Template"]) tmods

--
-- top-level template
--

genTLTemplateInterface :: TLTemplate -> [HsDecl GhcPs]
genTLTemplateInterface t =
  [ TyClD noExtField $
      mkClass cxEmpty (firstUpper (topleveltfunc_name t)) (map mkTBind tps) methods
  ]
  where
    tps = topleveltfunc_params t
    ctyp = cxx2HsType Nothing (topleveltfunc_ret t)
    lst = map (cxx2HsType Nothing . arg_type) (topleveltfunc_args t)
    sigdecl = mkFunSig (topleveltfunc_name t) $ foldr1 tyfun (lst <> [tyapp (tycon "IO") (tyParen ctyp)])
    methods = [sigdecl]

genTLTemplateImplementation :: TLTemplate -> [HsDecl GhcPs]
genTLTemplateImplementation t =
  mkFun nh sig (tvars_p ++ [p "suffix"]) rhs bstmts
  where
    v = mkVar
    p = mkPVar
    itps = zip ([1 ..] :: [Int]) (topleveltfunc_params t)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams = if nparams == 1 then tycon "Type" else tyTupleBoxed (replicate nparams (tycon "Type"))
    sig = foldr1 tyfun [tparams, tycon "String", tyapp (tycon "Q") (tycon "Exp")]
    tvars_p = if nparams == 1 then map p tvars else [pTuple (map p tvars)]
    prefix = "TL"
    nh = "t_" <> topleveltfunc_name t
    nc = topleveltfunc_name t
    lit' = strE (prefix <> "_" <> nc)
    lam = lamE [p "n"] (lit' `app` v "<>" `app` v "n")
    rhs =
      app (v "mkTFunc") $
        let typs = if nparams == 1 then map v tvars else [tupleE (map v tvars)]
         in tupleE (typs ++ [v "suffix", lam, v "tyf"])
    sig' =
      let e = error "genTLTemplateImplementation"
          spls = map (tySplice . parenSplice . mkVar) $ topleveltfunc_params t
          ctyp = cxx2HsType4Tmpl e Nothing spls (topleveltfunc_ret t)
          lst = map (cxx2HsType4Tmpl e Nothing spls . arg_type) (topleveltfunc_args t)
       in foldr1 tyfun (lst <> [tyapp (tycon "IO") (tyParen ctyp)])
    tassgns =
      fmap
        (\(i, tp) -> pbind_ (p tp) (v "pure" `app` (v ("typ" ++ show i))))
        itps
    bstmts =
      toLocalBinds True $
        valBinds
          [ mkBind1_
              "tyf"
              [wildcard]
              ( letE
                  (toLocalBinds False (valBinds tassgns))
                  (bracketExp (typeBracket sig'))
              )
          ]

genTLTemplateInstance ::
  TopLevelImportHeader ->
  TLTemplate ->
  [HsDecl GhcPs]
genTLTemplateInstance tih t =
  mkFun_
    fname
    sig
    (p "isCprim" : zipWith (\x y -> pTuple [p x, p y]) qtvars pvars)
    rhs
  where
    p = mkPVar
    v = mkVar
    tcname = firstUpper (topleveltfunc_name t)
    fname = "gen" <> tcname <> "InstanceFor"
    itps = zip ([1 ..] :: [Int]) (topleveltfunc_params t)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    qtvars = map (\(i, _) -> "qtyp" ++ show i) itps
    pvars = map (\(i, _) -> "param" ++ show i) itps
    nparams = length itps
    typs_v = if nparams == 1 then v (tvars !! 0) else tupleE (map v tvars)
    params_l = listE (map v pvars)
    sig =
      foldr1 tyfun $
        [tycon "IsCPrimitive"]
          ++ replicate
            nparams
            (tyTupleBoxed [tycon "Q" `tyapp` tycon "Type", tycon "TemplateParamInfo"])
          ++ [tycon "Q" `tyapp` tylist (tycon "Dec")]
    -- nvfs = zip ([1..] :: [Int]) vfs

    --------------------------
    -- final RHS expression --
    --------------------------
    rhs =
      doE
        ( [paramsstmt, suffixstmt]
            <> [ mkBindStmt (p "callmod_") (v "fmap" `app` v "loc_module" `app` (v "location")),
                 mkLetStmt
                   [ pbind_
                       (p "callmod")
                       (v "dot2_" `app` v "callmod_")
                   ]
               ]
            <> map genqtypstmt (zip tvars qtvars)
            <> [genstmt "f" (1 :: Int)]
            <> [ foreignSrcStmt,
                 mkLetStmt lststmt,
                 mkBodyStmt retstmt
               ]
        )
    --------------------------
    paramsstmt =
      mkLetStmt
        [ pbind_
            (p "params")
            (v "map" `app` (v "tpinfoSuffix") `app` params_l)
        ]
    suffixstmt =
      mkLetStmt
        [ pbind_
            (p "suffix")
            ( v "concatMap"
                `app` (lamE [p "x"] (inapp (strE "_") (op "++") (v "tpinfoSuffix" `app` v "x")))
                `app` params_l
            )
        ]
    genqtypstmt (tvar, qtvar) = mkBindStmt (p tvar) (v qtvar)
    genstmt prefix n =
      mkBindStmt
        (p (prefix <> show n))
        ( v "mkFunc"
            `app` strE (topleveltfunc_name t)
            `app` v ("t_" <> topleveltfunc_name t)
            `app` typs_v
            `app` v "suffix"
        )
    lststmt = [pbind_ (p "lst") (listE [v "f1"])]
    -- TODO: refactor out the following code.
    foreignSrcStmt =
      mkBodyStmt $
        (v "addModFinalizer")
          `app` par
            ( v "addForeignSource"
                `app` con "LangCxx"
                `app` par
                  ( L.foldr1
                      (\x y -> inapp x (op "++") y)
                      [ includeStatic,
                        {-                        , includeDynamic
                                                , namespaceStr -}
                        strE (tcname <> "_instance"),
                        par $
                          caseE
                            (v "isCprim")
                            [ (p "CPrim", strE "_s"),
                              (p "NonCPrim", strE "")
                            ],
                        strE "(",
                        v "intercalate"
                          `app` strE ", "
                          `app` par (inapp (v "callmod") (op ":") (v "params")),
                        strE ")\n"
                      ]
                  )
            )
      where
        -- temporary
        includeStatic =
          strE $
            concatMap
              (<> "\n")
              ( [R.renderCMacro (R.Include (HdrName "MacroPatternMatch.h"))]
                  ++ map
                    R.renderCMacro
                    ( map R.Include (tihExtraHeadersInCPP tih)
                        ++ [genTLTmplFunCpp CPrim t, genTLTmplFunCpp NonCPrim t]
                    )
              )
    {-
    cxxHeaders = v "concatMap" `app` (v "tpinfoCxxHeaders") `app` params_l
    cxxNamespaces = v "concatMap" `app` (v "tpinfoCxxNamespaces") `app` params_l

    includeDynamic =
      letE
        [ pbind_ (p "headers") cxxHeaders,
          pbind_
            (pApp (name "f") [p "x"])
            (v "renderCMacro" `app` (con "Include" `app` v "x"))
        ]
        (v "concatMap" `app` v "f" `app` v "headers")

    namespaceStr =
      letE
        [ pbind_ (p "nss") cxxNamespaces,
          pbind_
            (pApp (name "f") [p "x"])
            (v "renderCStmt" `app` (con "UsingNamespace" `app` v "x"))
        ]
        (v "concatMap" `app` v "f" `app` v "nss")
    -}
    retstmt =
      v "pure"
        `app` listE
          [ v "mkInstance"
              `app` listE []
              -- `app` (v "con" `app` strE tcname)
              `app` par
                ( foldl1
                    (\f x -> con "AppT" `app` f `app` x)
                    (par (v "con" `app` strE tcname) : map v tvars)
                )
              `app` (v "lst")
          ]
