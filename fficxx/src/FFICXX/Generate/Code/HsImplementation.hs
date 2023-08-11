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
    genTMFExp,
    genTMFInstance,
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
    cxEmpty,
    instD,
    mkBind1,
    mkFun,
    mkImport,
    mkInstance,
    mkVar,
  )
import qualified FFICXX.Generate.Util.HaskellSrcExts as O hiding (app, doE, listE, qualStmt, strE)
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import GHC.Hs (GhcPs)
import qualified Language.Haskell.Exts.Build as O hiding (op)
import qualified Language.Haskell.Exts.Syntax as O
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
          defn f = mkBind1 (hsFuncName child f) [] rhs Nothing
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
     in mkFun (aliasedFuncName c f) (functionSignature' c f) [] rhs Nothing

genHsFrontInstNonVirtual :: Class -> [HsDecl GhcPs]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature' c f) [] rhs Nothing
  where
    nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

genHsFrontInstStatic :: Class -> [HsDecl GhcPs]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature' c f) [] rhs Nothing

genHsFrontInstVariables :: Class -> [HsDecl GhcPs]
genHsFrontInstVariables c =
  flip concatMap (class_vars c) $ \v ->
    let rhs accessor =
          app
            (mkVar (case accessor of Getter -> "xform0"; _ -> "xform1"))
            (mkVar (hscAccessorName c v accessor))
     in mkFun (accessorName c v Getter) (accessorSignature c v Getter) [] (rhs Getter) Nothing
          <> mkFun (accessorName c v Setter) (accessorSignature c v Setter) [] (rhs Setter) Nothing

--
-- Template Member Function
--

genTemplateMemberFunctions :: ClassImportHeader -> [O.Decl ()]
genTemplateMemberFunctions cih =
  let c = cihClass cih
   in concatMap (\f -> genTMFExp c f <> genTMFInstance cih f) (class_tmpl_funcs c)

-- TODO: combine this with genTmplInstance
genTMFExp :: Class -> TemplateMemberFunction -> [O.Decl ()]
genTMFExp c f = O.mkFun nh sig (tvars_p ++ [p "suffix"]) rhs (Just bstmts)
  where
    nh = hsTemplateMemberFunctionNameTH c f
    v = O.mkVar
    p = O.mkPVar
    itps = zip ([1 ..] :: [Int]) (tmf_params f)
    tvars = map (\(i, _) -> "typ" ++ show i) itps
    nparams = length itps
    tparams = if nparams == 1 then O.tycon "Type" else O.tyTupleBoxed (replicate nparams (O.tycon "Type"))
    sig = foldr1 O.tyfun [tparams, O.tycon "String", O.tyapp (O.tycon "Q") (O.tycon "Exp")]
    tvars_p = if nparams == 1 then map p tvars else [O.pTuple (map p tvars)]
    lit' = O.strE (hsTemplateMemberFunctionName c f <> "_")
    lam = O.lamE [p "n"] (lit' `O.app` v "<>" `O.app` v "n")
    rhs =
      O.app (v "mkTFunc") $
        let typs = if nparams == 1 then map v tvars else [O.tuple (map v tvars)]
         in O.tuple (typs ++ [v "suffix", lam, v "tyf"])
    sig' = functionSignatureTMF c f
    tassgns = map (\(i, tp) -> O.pbind_ (p tp) (v "pure" `O.app` (v ("typ" ++ show i)))) itps
    bstmts =
      O.binds
        [ O.mkBind1
            "tyf"
            [O.mkPVar "n"]
            ( O.letE
                tassgns
                (O.bracketExp (O.typeBracket sig'))
            )
            Nothing
        ]

genTMFInstance :: ClassImportHeader -> TemplateMemberFunction -> [O.Decl ()]
genTMFInstance cih f =
  O.mkFun
    fname
    sig
    [p "isCprim", O.pTuple [p "qtyp", p "param"]]
    rhs
    Nothing
  where
    c = cihClass cih
    fname = "genInstanceFor_" <> hsTemplateMemberFunctionName c f
    p = O.mkPVar
    v = O.mkVar
    sig =
      O.tycon "IsCPrimitive"
        `O.tyfun` O.tyTupleBoxed [O.tycon "Q" `O.tyapp` O.tycon "Type", O.tycon "TemplateParamInfo"]
        `O.tyfun` (O.tycon "Q" `O.tyapp` O.tylist (O.tycon "Dec"))
    rhs = O.doE [suffixstmt, qtypstmt, genstmt, foreignSrcStmt, O.letStmt lststmt, O.qualStmt retstmt]
    suffixstmt = O.letStmt [O.pbind_ (p "suffix") (v "tpinfoSuffix" `O.app` v "param")]
    qtypstmt = O.generator (p "typ") (v "qtyp")
    genstmt =
      O.generator
        (p "f1")
        ( v "mkMember"
            `O.app` ( O.strE (hsTemplateMemberFunctionName c f <> "_")
                        `O.app` v "<>"
                        `O.app` v "suffix"
                    )
            `O.app` v (hsTemplateMemberFunctionNameTH c f)
            `O.app` v "typ"
            `O.app` v "suffix"
        )
    lststmt = [O.pbind_ (p "lst") (O.listE ([v "f1"]))]
    retstmt = v "pure" `O.app` v "lst"
    -- TODO: refactor out the following code.
    foreignSrcStmt =
      O.qualifier $
        (v "addModFinalizer")
          `O.app` ( v "addForeignSource"
                      `O.app` O.con "LangCxx"
                      `O.app` ( L.foldr1
                                  (\x y -> O.inapp x (O.op "++") y)
                                  [ includeStatic,
                                    includeDynamic,
                                    namespaceStr,
                                    O.strE (hsTemplateMemberFunctionName c f),
                                    O.strE "(",
                                    v "suffix",
                                    O.strE ")\n"
                                  ]
                              )
                  )
      where
        includeStatic =
          O.strE $
            concatMap ((<> "\n") . R.renderCMacro . R.Include) $
              [HdrName "MacroPatternMatch.h", cihSelfHeader cih]
                <> cihIncludedHPkgHeadersInCPP cih
                <> cihIncludedCPkgHeaders cih
        includeDynamic =
          O.letE
            [ O.pbind_ (p "headers") (v "tpinfoCxxHeaders" `O.app` v "param"),
              O.pbind_
                (O.pApp (O.name "f") [p "x"])
                (v "renderCMacro" `O.app` (O.con "Include" `O.app` v "x"))
            ]
            (v "concatMap" `O.app` v "f" `O.app` v "headers")
        namespaceStr =
          O.letE
            [ O.pbind_ (p "nss") (v "tpinfoCxxNamespaces" `O.app` v "param"),
              O.pbind_
                (O.pApp (O.name "f") [p "x"])
                (v "renderCStmt" `O.app` (O.con "UsingNamespace" `O.app` v "x"))
            ]
            (v "concatMap" `O.app` v "f" `O.app` v "nss")
