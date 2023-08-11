{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsTH
  ( genImportInTH,
    genTmplImplementation,
    genTmplInstance,
  )
where

import qualified Data.List as L (foldr1)
import FFICXX.Generate.Code.Cpp
  ( genTmplClassCpp,
    genTmplFunCpp,
    genTmplVarCpp,
  )
import FFICXX.Generate.Code.Primitive
  ( functionSignatureTT,
    tmplAccessorToTFun,
  )
import FFICXX.Generate.Dependency (calculateDependency)
import FFICXX.Generate.Name
  ( ffiTmplFuncName,
    hsTmplFuncName,
    hsTmplFuncNameTH,
    subModuleName,
    tmplAccessorName,
    typeclassNameT,
  )
import FFICXX.Generate.Type.Class
  ( Accessor (Getter, Setter),
    Arg (..),
    TemplateClass (..),
    TemplateFunction (..),
    Types (Void),
    Variable (..),
  )
import FFICXX.Generate.Type.Module
  ( TemplateClassImportHeader (..),
    TemplateClassSubmoduleType (..),
  )
import FFICXX.Generate.Util.HaskellSrcExts
  ( bracketExp,
    con,
    generator,
    inapp,
    match,
    mkBind1,
    mkFun,
    mkImport,
    mkPVar,
    mkVar,
    op,
    pbind_,
    qualifier,
    tyTupleBoxed,
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
import Language.Haskell.Exts.Syntax
  ( Decl,
    ImportDecl,
  )

genImportInTH :: TemplateClass -> [ImportDecl ()]
genImportInTH t0 =
  fmap (mkImport . subModuleName) $ calculateDependency $ Left (TCSTTH, t0)

--
-- implementation
--

genTmplImplementation :: TemplateClass -> [Decl ()]
genTmplImplementation t =
  concatMap gen (tclass_funcs t) ++ concatMap genV (tclass_vars t)
  where
    v = mkVar
    p = mkPVar
    itps = zip ([1 ..] :: [Int]) (tclass_params t)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams = if nparams == 1 then tycon "Type" else tyTupleBoxed (replicate nparams (tycon "Type"))
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
            (tyTupleBoxed [tycon "Q" `tyapp` tycon "Type", tycon "TemplateParamInfo"])
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
        ( v nm
            `app` strE (hsTmplFuncName t f)
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
