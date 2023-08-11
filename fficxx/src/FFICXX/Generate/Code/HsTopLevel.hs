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
    convertCpp2HS,
    convertCpp2HS4Tmpl,
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
{- import FFICXX.Generate.Type.Class
  ( Class (..),
    TemplateClass (..),
    TemplateFunction (..),
    TemplateMemberFunction (..),
    Types (Void),
    Variable (..),
  ) -}
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
import FFICXX.Generate.Util.HaskellSrcExts
  ( bracketExp,
    clsDecl,
    con,
    cxEmpty,
    cxTuple,
    eabs,
    ethingall,
    evar,
    generator,
    inapp,
    listE,
    match,
    mkBind1,
    mkClass,
    mkFun,
    mkFunSig,
    mkImport,
    mkPVar,
    mkTBind,
    mkVar,
    nonamespace,
    op,
    parenSplice,
    pbind_,
    qualifier,
    strE,
    tyForall,
    tySplice,
    tyTupleBoxed,
    tyapp,
    tycon,
    tyfun,
    tylist,
    typeBracket,
    unqual,
  )
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import FFICXX.Runtime.TH (IsCPrimitive (CPrim, NonCPrim))
import Language.Haskell.Exts.Build
  ( app,
    binds,
    caseE,
    doE,
    lamE,
    letE,
    letStmt,
    pTuple,
    paren,
    qualStmt,
    tuple,
    wildcard,
  )
import Language.Haskell.Exts.Syntax
  ( Decl,
    ExportSpec,
    ImportDecl,
  )
import System.FilePath ((<.>))

--
-- exports
--

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

--
-- imports
--

-- | module summary re-exports
genImportInModule :: Class -> [ImportDecl ()]
genImportInModule x = map (\y -> mkImport (getClassModuleBase x <.> y)) ["RawType", "Interface", "Implementation"]

-- | top=level
genImportInTopLevel ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  [ImportDecl ()]
genImportInTopLevel modname (mods, _tmods) =
  map (mkImport . cmModule) mods
    ++ map mkImport [modname <.> "Template", modname <.> "TH", modname <.> "Ordinary"]

--
-- declarations and definitions
--

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

--
-- top-level template
--

genTLTemplateInterface :: TLTemplate -> [Decl ()]
genTLTemplateInterface t =
  [ mkClass cxEmpty (firstUpper (topleveltfunc_name t)) (map mkTBind tps) methods
  ]
  where
    tps = topleveltfunc_params t
    ctyp = convertCpp2HS Nothing (topleveltfunc_ret t)
    lst = map (convertCpp2HS Nothing . arg_type) (topleveltfunc_args t)
    sigdecl = mkFunSig (topleveltfunc_name t) $ foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
    methods = [clsDecl sigdecl]

genTLTemplateImplementation :: TLTemplate -> [Decl ()]
genTLTemplateImplementation t =
  mkFun nh sig (tvars_p ++ [p "suffix"]) rhs (Just bstmts)
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
        let typs = if nparams == 1 then map v tvars else [tuple (map v tvars)]
         in tuple (typs ++ [v "suffix", lam, v "tyf"])
    sig' =
      let e = error "genTLTemplateImplementation"
          spls = map (tySplice . parenSplice . mkVar) $ topleveltfunc_params t
          ctyp = convertCpp2HS4Tmpl e Nothing spls (topleveltfunc_ret t)
          lst = map (convertCpp2HS4Tmpl e Nothing spls . arg_type) (topleveltfunc_args t)
       in foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
    tassgns = map (\(i, tp) -> pbind_ (p tp) (v "pure" `app` (v ("typ" ++ show i)))) itps
    bstmts =
      binds
        [ mkBind1
            "tyf"
            [wildcard]
            ( letE
                tassgns
                (bracketExp (typeBracket sig'))
            )
            Nothing
        ]

genTLTemplateInstance ::
  TopLevelImportHeader ->
  TLTemplate ->
  [Decl ()]
genTLTemplateInstance tih t =
  mkFun
    fname
    sig
    (p "isCprim" : zipWith (\x y -> pTuple [p x, p y]) qtvars pvars)
    rhs
    Nothing
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
    typs_v = if nparams == 1 then v (tvars !! 0) else tuple (map v tvars)
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
            <> [ generator (p "callmod_") (v "fmap" `app` v "loc_module" `app` (v "location")),
                 letStmt
                   [ pbind_
                       (p "callmod")
                       (v "dot2_" `app` v "callmod_")
                   ]
               ]
            <> map genqtypstmt (zip tvars qtvars)
            <> [genstmt "f" (1 :: Int)]
            <> [ foreignSrcStmt,
                 letStmt lststmt,
                 qualStmt retstmt
               ]
        )
    --------------------------
    paramsstmt =
      letStmt
        [ pbind_
            (p "params")
            (v "map" `app` (v "tpinfoSuffix") `app` params_l)
        ]
    suffixstmt =
      letStmt
        [ pbind_
            (p "suffix")
            ( v "concatMap"
                `app` (lamE [p "x"] (inapp (strE "_") (op "++") (v "tpinfoSuffix" `app` v "x")))
                `app` params_l
            )
        ]
    genqtypstmt (tvar, qtvar) = generator (p tvar) (v qtvar)
    genstmt prefix n =
      generator
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
      qualifier $
        (v "addModFinalizer")
          `app` ( v "addForeignSource"
                    `app` con "LangCxx"
                    `app` ( L.foldr1
                              (\x y -> inapp x (op "++") y)
                              [ includeStatic,
                                {-                        , includeDynamic
                                                        , namespaceStr -}
                                strE (tcname <> "_instance"),
                                paren $
                                  caseE
                                    (v "isCprim")
                                    [ match (p "CPrim") (strE "_s"),
                                      match (p "NonCPrim") (strE "")
                                    ],
                                strE "(",
                                v "intercalate"
                                  `app` strE ", "
                                  `app` paren (inapp (v "callmod") (op ":") (v "params")),
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
              `app` foldl1
                (\f x -> con "AppT" `app` f `app` x)
                (v "con" `app` strE tcname : map v tvars)
              `app` (v "lst")
          ]
