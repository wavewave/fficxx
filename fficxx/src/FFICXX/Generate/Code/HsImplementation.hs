{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsImplementation
  ( -- * import
    genImportInImplementation,

    -- * functions
    genHsFrontInst,
    genHsFrontInstNew,
    genHsFrontInstNonVirtual,
    genHsFrontInstStatic,
    genHsFrontInstVariables,

    -- * template member functions
    genTemplateMemberFunctions,
  )
where

import Control.Monad.Reader (Reader)
import qualified Data.List as L (foldr1)
import FFICXX.Generate.Code.Primitive
  ( accessorSignature,
    cxx2HsType,
    functionSignature',
    functionSignatureTMF,
    hsFuncXformer,
  )
import FFICXX.Generate.Name
  ( accessorName,
    aliasedFuncName,
    hsFuncName,
    hsTemplateMemberFunctionName,
    hsTemplateMemberFunctionNameTH,
    hscAccessorName,
    hscFuncName,
    subModuleName,
    typeclassName,
  )
import FFICXX.Generate.Type.Annotate (AnnotateMap)
import FFICXX.Generate.Type.Class
  ( Accessor (..),
    Class (..),
    TemplateMemberFunction (..),
    Types (..),
    isAbstractClass,
    isNewFunc,
    nonVirtualNotNewFuncs,
    staticFuncs,
    virtualFuncs,
  )
import FFICXX.Generate.Type.Module
  ( ClassImportHeader (..),
    ClassModule (..),
  )
import FFICXX.Generate.Util.GHCExactPrint
  ( app,
    bracketExp,
    con,
    cxEmpty,
    doE,
    inapp,
    instD,
    lamE,
    letE,
    listE,
    mkBind1_,
    mkBindStmt,
    mkBodyStmt,
    mkFun,
    mkFun_,
    mkImport,
    mkInstance,
    mkLetStmt,
    mkPVar,
    mkVar,
    op,
    pApp,
    pTuple,
    par,
    pbind_,
    strE,
    toLocalBinds,
    tupleE,
    tyTupleBoxed,
    tyapp,
    tycon,
    tyfun,
    tylist,
    typeBracket,
    valBinds,
  )
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax (HsDecl, ImportDecl)

--
-- import
--

genImportInImplementation :: ClassModule -> [ImportDecl GhcPs]
genImportInImplementation m =
  fmap (mkImport . subModuleName) $ cmImportedSubmodulesForImplementation m

--
-- functions
--

genHsFrontInst :: Class -> Class -> [HsDecl GhcPs]
genHsFrontInst parent child
  | (not . isAbstractClass) child =
      let idecl = mkInstance cxEmpty (typeclassName parent) [cxx2HsType (Just child) SelfType] [] body
          defn f = mkBind1_ (hsFuncName child f) [] rhs
            where
              rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName child f))
          body = map defn . virtualFuncs . class_funcs $ parent
       in [instD idecl]
  | otherwise = []

genHsFrontInstNew ::
  -- | only concrete class
  Class ->
  Reader AnnotateMap [HsDecl GhcPs]
genHsFrontInstNew c = do
  -- amap <- ask
  let fs = filter isNewFunc (class_funcs c)
  pure . flip concatMap fs $ \f ->
    let -- for the time being, let's ignore annotation.
        -- cann = maybe "" id $ M.lookup (PkgMethod, constructorName c) amap
        -- newfuncann = mkComment 0 cann
        rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun_ (aliasedFuncName c f) (functionSignature' c f) [] rhs

genHsFrontInstNonVirtual :: Class -> [HsDecl GhcPs]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun_ (aliasedFuncName c f) (functionSignature' c f) [] rhs
  where
    nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

genHsFrontInstStatic :: Class -> [HsDecl GhcPs]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun_ (aliasedFuncName c f) (functionSignature' c f) [] rhs

genHsFrontInstVariables :: Class -> [HsDecl GhcPs]
genHsFrontInstVariables c =
  flip concatMap (class_vars c) $ \v ->
    let rhs accessor =
          app
            (mkVar (case accessor of Getter -> "xform0"; _ -> "xform1"))
            (mkVar (hscAccessorName c v accessor))
     in mkFun_ (accessorName c v Getter) (accessorSignature c v Getter) [] (rhs Getter)
          <> mkFun_ (accessorName c v Setter) (accessorSignature c v Setter) [] (rhs Setter)

--
-- Template Member Function
--

genTemplateMemberFunctions :: ClassImportHeader -> [HsDecl GhcPs]
genTemplateMemberFunctions cih =
  let c = cihClass cih
   in concatMap (\f -> genTMFExp c f <> genTMFInstance cih f) (class_tmpl_funcs c)

-- TODO: combine this with genTmplInstance
genTMFExp :: Class -> TemplateMemberFunction -> [HsDecl GhcPs]
genTMFExp c f = mkFun nh sig (tvars_p ++ [p "suffix"]) rhs bstmts
  where
    nh = hsTemplateMemberFunctionNameTH c f
    v = mkVar
    p = mkPVar
    itps = zip ([1 ..] :: [Int]) (tmf_params f)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams
      | nparams == 1 = tycon "Type"
      | otherwise = tyTupleBoxed (replicate nparams (tycon "Type"))
    sig = foldr1 tyfun [tparams, tycon "String", tyapp (tycon "Q") (tycon "Exp")]
    tvars_p
      | nparams == 1 = fmap p tvars
      | otherwise = [pTuple (fmap p tvars)]
    lit' = strE (hsTemplateMemberFunctionName c f <> "_")
    lam = lamE [p "n"] (lit' `app` v "<>" `app` v "n")
    rhs =
      app (v "mkTFunc") $
        let typs
              | nparams == 1 = fmap v tvars
              | otherwise = [tupleE (map v tvars)]
         in tupleE (typs ++ [v "suffix", lam, v "tyf"])
    sig' = functionSignatureTMF c f
    tassgns =
      fmap
        (\(i, tp) -> pbind_ (p tp) (v "pure" `app` (v ("typ" ++ show i))))
        itps
    bstmts =
      toLocalBinds True $
        valBinds
          [ mkBind1_
              "tyf"
              [mkPVar "n"]
              ( letE
                  (toLocalBinds False (valBinds tassgns))
                  (bracketExp (typeBracket sig'))
              )
          ]

genTMFInstance :: ClassImportHeader -> TemplateMemberFunction -> [HsDecl GhcPs]
genTMFInstance cih f =
  mkFun_
    fname
    sig
    [p "isCprim", pTuple [p "qtyp", p "param"]]
    rhs
  where
    c = cihClass cih
    fname = "genInstanceFor_" <> hsTemplateMemberFunctionName c f
    p = mkPVar
    v = mkVar
    sig =
      tycon "IsCPrimitive"
        `tyfun` tyTupleBoxed [tycon "Q" `tyapp` tycon "Type", tycon "TemplateParamInfo"]
        `tyfun` (tycon "Q" `tyapp` tylist (tycon "Dec"))
    rhs =
      doE
        [ suffixstmt,
          qtypstmt,
          genstmt,
          foreignSrcStmt,
          mkLetStmt lststmt,
          mkBodyStmt retstmt
        ]
    suffixstmt =
      mkLetStmt [pbind_ (p "suffix") (v "tpinfoSuffix" `app` v "param")]
    qtypstmt =
      mkBindStmt (p "typ") (v "qtyp")
    genstmt =
      mkBindStmt
        (p "f1")
        ( v "mkMember"
            `app` par
              ( strE (hsTemplateMemberFunctionName c f <> "_")
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
      mkBodyStmt $
        (v "addModFinalizer")
          `app` par
            ( v "addForeignSource"
                `app` con "LangCxx"
                `app` par
                  ( L.foldr1
                      (\x y -> inapp x (op "++") y)
                      [ par includeStatic,
                        par includeDynamic,
                        par namespaceStr,
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
            ( toLocalBinds False . valBinds $
                [ pbind_ (p "headers") (v "tpinfoCxxHeaders" `app` v "param"),
                  pbind_
                    (pApp "f" [p "x"])
                    (v "renderCMacro" `app` par (con "Include" `app` v "x"))
                ]
            )
            (v "concatMap" `app` v "f" `app` v "headers")
        namespaceStr =
          letE
            ( toLocalBinds False . valBinds $
                [ pbind_ (p "nss") (v "tpinfoCxxNamespaces" `app` v "param"),
                  pbind_
                    (pApp "f" [p "x"])
                    (v "renderCStmt" `app` par (con "UsingNamespace" `app` v "x"))
                ]
            )
            (v "concatMap" `app` v "f" `app` v "nss")
