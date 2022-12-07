{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsTemplate where

import qualified Data.List as L (foldr1)
import FFICXX.Generate.Code.Cpp
  ( genTLTmplFunCpp,
    genTmplClassCpp,
    genTmplFunCpp,
    genTmplVarCpp,
  )
import FFICXX.Generate.Code.HsCast (castBody)
import FFICXX.Generate.Code.Primitive
  ( convertCpp2HS,
    convertCpp2HS4Tmpl,
    functionSignatureT,
    functionSignatureTMF,
    functionSignatureTT,
    tmplAccessorToTFun,
  )
import FFICXX.Generate.Dependency
  ( calculateDependency,
    mkModuleDepInplace,
  )
import FFICXX.Generate.Name
  ( ffiTmplFuncName,
    getClassModuleBase,
    getTClassModuleBase,
    hsTemplateClassName,
    hsTemplateMemberFunctionName,
    hsTemplateMemberFunctionNameTH,
    hsTmplFuncName,
    hsTmplFuncNameTH,
    subModuleName,
    tmplAccessorName,
    typeclassNameT,
  )
import FFICXX.Generate.Type.Class
  ( Accessor (Getter, Setter),
    Arg (..),
    Class (..),
    TLTemplate (..),
    TemplateClass (..),
    TemplateFunction (..),
    TemplateMemberFunction (..),
    Types (Void),
    Variable (..),
  )
import FFICXX.Generate.Type.Module
  ( ClassImportHeader (..),
    TemplateClassImportHeader (..),
    TemplateClassSubmoduleType (..),
    TopLevelImportHeader (..),
  )
import FFICXX.Generate.Util (firstUpper)
import FFICXX.Generate.Util.HaskellSrcExts
  ( bracketExp,
    clsDecl,
    con,
    conDecl,
    cxEmpty,
    generator,
    inapp,
    insDecl,
    insType,
    match,
    mkBind1,
    mkClass,
    mkData,
    mkFun,
    mkFunSig,
    mkImport,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkTBind,
    mkTVar,
    mkVar,
    op,
    parenSplice,
    pbind_,
    qualConDecl,
    qualifier,
    tyPtr,
    tySplice,
    tyapp,
    tycon,
    tyfun,
    tylist,
    typeBracket,
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
    listE,
    name,
    pApp,
    pTuple,
    paren,
    qualStmt,
    strE,
    tuple,
    wildcard,
  )
import Language.Haskell.Exts.Syntax (Boxed (Boxed), Decl (..), ImportDecl (..), Type (TyTuple))
import System.FilePath ((<.>))

------------------------------
-- Template member function --
------------------------------

genTemplateMemberFunctions :: ClassImportHeader -> [Decl ()]
genTemplateMemberFunctions cih =
  let c = cihClass cih
   in concatMap (\f -> genTMFExp c f <> genTMFInstance cih f) (class_tmpl_funcs c)

-- TODO: combine this with genTmplInstance
genTMFExp :: Class -> TemplateMemberFunction -> [Decl ()]
genTMFExp c f = mkFun nh sig (tvars_p ++ [p "suffix"]) rhs (Just bstmts)
  where
    nh = hsTemplateMemberFunctionNameTH c f
    v = mkVar
    p = mkPVar
    itps = zip ([1 ..] :: [Int]) (tmf_params f)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams = if nparams == 1 then tycon "Type" else TyTuple () Boxed (replicate nparams (tycon "Type"))
    sig = foldr1 tyfun [tparams, tycon "String", tyapp (tycon "Q") (tycon "Exp")]
    tvars_p = if nparams == 1 then map p tvars else [pTuple (map p tvars)]
    lit' = strE (hsTemplateMemberFunctionName c f <> "_")
    lam = lamE [p "n"] (lit' `app` v "<>" `app` v "n")
    rhs =
      app (v "mkTFunc") $
        let typs = if nparams == 1 then map v tvars else [tuple (map v tvars)]
         in tuple (typs ++ [v "suffix", lam, v "tyf"])
    sig' = functionSignatureTMF c f
    tassgns = map (\(i, tp) -> pbind_ (p tp) (v "pure" `app` (v ("typ" ++ show i)))) itps
    bstmts =
      binds
        [ mkBind1
            "tyf"
            [mkPVar "n"]
            ( letE
                tassgns
                (bracketExp (typeBracket sig'))
            )
            Nothing
        ]

genTMFInstance :: ClassImportHeader -> TemplateMemberFunction -> [Decl ()]
genTMFInstance cih f =
  mkFun
    fname
    sig
    [p "isCprim", pTuple [p "qtyp", p "param"]]
    rhs
    Nothing
  where
    c = cihClass cih
    fname = "genInstanceFor_" <> hsTemplateMemberFunctionName c f
    p = mkPVar
    v = mkVar
    sig =
      tycon "IsCPrimitive"
        `tyfun` TyTuple () Boxed [tycon "Q" `tyapp` tycon "Type", tycon "TemplateParamInfo"]
        `tyfun` (tycon "Q" `tyapp` tylist (tycon "Dec"))
    rhs = doE [suffixstmt, qtypstmt, genstmt, foreignSrcStmt, letStmt lststmt, qualStmt retstmt]
    suffixstmt = letStmt [pbind_ (p "suffix") (v "tpinfoSuffix" `app` v "param")]
    qtypstmt = generator (p "typ") (v "qtyp")
    genstmt =
      generator
        (p "f1")
        ( v "mkMember"
            `app` ( strE (hsTemplateMemberFunctionName c f <> "_")
                      `app` v "<>"
                      `app` v "suffix"
                  )
            `app` v (hsTemplateMemberFunctionNameTH c f)
            `app` v "typ"
            `app` v "suffix"
        )
    lststmt = [pbind_ (p "lst") (listE ([v "f1"]))]
    retstmt = v "pure" `app` v "lst"
    -- TODO: refactor out the following code.
    foreignSrcStmt =
      qualifier $
        (v "addModFinalizer")
          `app` ( v "addForeignSource"
                    `app` con "LangCxx"
                    `app` ( L.foldr1
                              (\x y -> inapp x (op "++") y)
                              [ includeStatic,
                                includeDynamic,
                                namespaceStr,
                                strE (hsTemplateMemberFunctionName c f),
                                strE "(",
                                v "suffix",
                                strE ")\n"
                              ]
                          )
                )
      where
        includeStatic =
          strE $
            concatMap ((<> "\n") . R.renderCMacro . R.Include) $
              [HdrName "MacroPatternMatch.h", cihSelfHeader cih]
                <> cihIncludedHPkgHeadersInCPP cih
                <> cihIncludedCPkgHeaders cih
        includeDynamic =
          letE
            [ pbind_ (p "headers") (v "tpinfoCxxHeaders" `app` v "param"),
              pbind_
                (pApp (name "f") [p "x"])
                (v "renderCMacro" `app` (con "Include" `app` v "x"))
            ]
            (v "concatMap" `app` v "f" `app` v "headers")
        namespaceStr =
          letE
            [ pbind_ (p "nss") (v "tpinfoCxxNamespaces" `app` v "param"),
              pbind_
                (pApp (name "f") [p "x"])
                (v "renderCStmt" `app` (con "UsingNamespace" `app` v "x"))
            ]
            (v "concatMap" `app` v "f" `app` v "nss")

--------------------
-- Template Class --
--------------------

genImportInTemplate :: TemplateClass -> [ImportDecl ()]
genImportInTemplate t0 =
  let depsRaw =
        fmap (mkImport . subModuleName) $
          calculateDependency $
            Left (TCSTTemplate, t0)
      depsInplace = mkModuleDepInplace (Left t0)
   in depsRaw
        <> flip
          map
          depsInplace
          ( \case
              Left t -> mkImport (getTClassModuleBase t <.> "Template")
              Right c -> mkImport (getClassModuleBase c <.> "Interface")
          )

-- |
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
    mkInstance cxEmpty "Castable" [hightype, tyapp tyPtr rawtype] castBody
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

-- |
genImportInTH :: TemplateClass -> [ImportDecl ()]
genImportInTH t0 =
  let depsRaw = fmap (mkImport . subModuleName) $ calculateDependency $ Left (TCSTTH, t0)
      depsInplace = mkModuleDepInplace (Left t0)
   in depsRaw
        <> flip
          concatMap
          depsInplace
          ( \case
              Left t -> [mkImport (getTClassModuleBase t <.> "Template")]
              Right c -> map (\y -> mkImport (getClassModuleBase c <.> y)) ["RawType", "Cast", "Interface"]
          )

-- |
genTmplImplementation :: TemplateClass -> [Decl ()]
genTmplImplementation t =
  concatMap gen (tclass_funcs t) ++ concatMap genV (tclass_vars t)
  where
    v = mkVar
    p = mkPVar
    itps = zip ([1 ..] :: [Int]) (tclass_params t)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams = if nparams == 1 then tycon "Type" else TyTuple () Boxed (replicate nparams (tycon "Type"))
    sig = foldr1 tyfun [tparams, tycon "String", tyapp (tycon "Q") (tycon "Exp")]
    tvars_p = if nparams == 1 then map p tvars else [pTuple (map p tvars)]
    prefix = tclass_name t
    gen f = mkFun nh sig (tvars_p ++ [p "suffix"]) rhs (Just bstmts)
      where
        nh = hsTmplFuncNameTH t f
        nc = ffiTmplFuncName f
        lit' = strE (prefix <> "_" <> nc)
        lam = lamE [p "n"] (lit' `app` v "<>" `app` v "n")
        rhs =
          app (v "mkTFunc") $
            let typs = if nparams == 1 then map v tvars else [tuple (map v tvars)]
             in tuple (typs ++ [v "suffix", lam, v "tyf"])
        sig' = functionSignatureTT t f
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
    genV vf =
      let f_g = tmplAccessorToTFun vf Getter
          f_s = tmplAccessorToTFun vf Setter
       in gen f_g ++ gen f_s

-- |
genTmplInstance ::
  TemplateClassImportHeader ->
  [Decl ()]
genTmplInstance tcih =
  mkFun
    fname
    sig
    (p "isCprim" : zipWith (\x y -> pTuple [p x, p y]) qtvars pvars)
    rhs
    Nothing
  where
    t = tcihTClass tcih
    fs = tclass_funcs t
    vfs = tclass_vars t
    tname = tclass_name t
    fname = "gen" <> tname <> "InstanceFor"
    p = mkPVar
    v = mkVar
    itps = zip ([1 ..] :: [Int]) (tclass_params t)
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
            (TyTuple () Boxed [tycon "Q" `tyapp` tycon "Type", tycon "TemplateParamInfo"])
          ++ [tycon "Q" `tyapp` tylist (tycon "Dec")]
    nfs = zip ([1 ..] :: [Int]) fs
    nvfs = zip ([1 ..] :: [Int]) vfs
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
            <> map genstmt nfs
            <> concatMap genvarstmt nvfs
            <> [foreignSrcStmt, letStmt lststmt, qualStmt retstmt]
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
    gen prefix nm f n =
      generator
        (p (prefix <> show n))
        ( v nm `app` strE (hsTmplFuncName t f)
            `app` v (hsTmplFuncNameTH t f)
            `app` typs_v
            `app` v "suffix"
        )
    genstmt (n, f@TFun {}) = gen "f" "mkMember" f n
    genstmt (n, f@TFunNew {}) = gen "f" "mkNew" f n
    genstmt (n, f@TFunDelete) = gen "f" "mkDelete" f n
    genstmt (n, f@TFunOp {}) = gen "f" "mkMember" f n
    genvarstmt (n, vf) =
      let Variable (Arg {..}) = vf
          f_g =
            TFun
              { tfun_ret = arg_type,
                tfun_name = tmplAccessorName vf Getter,
                tfun_oname = tmplAccessorName vf Getter,
                tfun_args = []
              }
          f_s =
            TFun
              { tfun_ret = Void,
                tfun_name = tmplAccessorName vf Setter,
                tfun_oname = tmplAccessorName vf Setter,
                tfun_args = [Arg arg_type "value"]
              }
       in [ gen "vf" "mkMember" f_g (2 * n - 1),
            gen "vf" "mkMember" f_s (2 * n)
          ]
    lststmt =
      let mkElems prefix xs = map (v . (\n -> prefix <> show n) . fst) xs
       in [ pbind_
              (p "lst")
              ( listE
                  ( mkElems "f" nfs
                      <> mkElems "vf" (concatMap (\(n, vf) -> [(2 * n - 1, vf), (2 * n, vf)]) nvfs)
                  )
              )
          ]
    -- TODO: refactor out the following code.
    foreignSrcStmt =
      qualifier $
        (v "addModFinalizer")
          `app` ( v "addForeignSource"
                    `app` con "LangCxx"
                    `app` ( L.foldr1
                              (\x y -> inapp x (op "++") y)
                              [ includeStatic,
                                includeDynamic,
                                namespaceStr,
                                strE (tname <> "_instance"),
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
        body =
          map R.renderCMacro $
            map R.Include (tcihCxxHeaders tcih)
              ++ map (genTmplFunCpp NonCPrim t) fs
              ++ map (genTmplFunCpp CPrim t) fs
              ++ concatMap (genTmplVarCpp NonCPrim t) vfs
              ++ concatMap (genTmplVarCpp CPrim t) vfs
              ++ [ genTmplClassCpp NonCPrim t (fs, vfs),
                   genTmplClassCpp CPrim t (fs, vfs)
                 ]
        includeStatic =
          strE $
            concatMap
              (<> "\n")
              ( [R.renderCMacro (R.Include (HdrName "MacroPatternMatch.h"))]
                  ++ body
              )
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
    retstmt =
      v "pure"
        `app` listE
          [ v "mkInstance"
              `app` listE []
              `app` foldl1
                (\f x -> con "AppT" `app` f `app` x)
                (v "con" `app` strE (typeclassNameT t) : map v tvars)
              `app` (v "lst")
          ]

---------------
-- top-level --
---------------

-- |
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

-- |
genTLTemplateImplementation :: TLTemplate -> [Decl ()]
genTLTemplateImplementation t =
  mkFun nh sig (tvars_p ++ [p "suffix"]) rhs (Just bstmts)
  where
    v = mkVar
    p = mkPVar
    itps = zip ([1 ..] :: [Int]) (topleveltfunc_params t)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams = if nparams == 1 then tycon "Type" else TyTuple () Boxed (replicate nparams (tycon "Type"))
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
            (TyTuple () Boxed [tycon "Q" `tyapp` tycon "Type", tycon "TemplateParamInfo"])
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
        ( v "mkFunc" `app` strE (topleveltfunc_name t)
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
