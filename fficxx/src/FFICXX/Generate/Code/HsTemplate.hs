{-# LANGUAGE RecordWildCards   #-}

module FFICXX.Generate.Code.HsTemplate where

import Data.Monoid                    ( (<>) )
import qualified Data.List as L       ( foldr1 )
import Language.Haskell.Exts.Build    ( app, binds, caseE, doE
                                      , letE, letStmt, name, pApp, paren
                                      , qualStmt, strE, tuple
                                      )
import Language.Haskell.Exts.Syntax   ( Decl(..) )
--
import FFICXX.Runtime.CodeGen.Cxx     ( HeaderName(..) )
import qualified FFICXX.Runtime.CodeGen.Cxx as R
--
import FFICXX.Generate.Code.Primitive ( functionSignatureT
                                      , functionSignatureTT
                                      , functionSignatureTMF
                                      )
import FFICXX.Generate.Code.HsCast    ( castBody )
import FFICXX.Generate.Name           ( ffiTmplFuncName
                                      , hsTemplateClassName
                                      , hsTemplateMemberFunctionName
                                      , hsTemplateMemberFunctionNameTH
                                      , hsTmplFuncName
                                      , hsTmplFuncNameTH
                                      , typeclassNameT
                                      )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , TemplateClass(..)
                                      , TemplateFunction(..)
                                      , TemplateMemberFunction(..)
                                      )
import FFICXX.Generate.Type.Module    ( ClassImportHeader(..)
                                      , TemplateClassImportHeader(..)
                                      )
import FFICXX.Generate.Util.HaskellSrcExts
                                      ( bracketExp
                                      , con, conDecl, cxEmpty, clsDecl
                                      , generator
                                      , inapp, insDecl, insType
                                      , lambda, list
                                      , match, mkBind1, mkTBind, mkData, mkNewtype
                                      , mkFun, mkFunSig, mkClass, mkInstance
                                      , mkPVar, mkTVar, mkVar
                                      , op, pbind_
                                      , qualConDecl, qualifier
                                      , tyapp, tycon, tyfun, tylist, tyPtr
                                      , typeBracket
                                      )


------------------------------
-- Template member function --
------------------------------

genTemplateMemberFunctions :: ClassImportHeader -> [Decl ()]
genTemplateMemberFunctions cih =
  let c = cihClass cih 
  in concatMap (\f -> genTMFExp c f <> genTMFInstance cih f) (class_tmpl_funcs c)


genTMFExp :: Class -> TemplateMemberFunction -> [Decl ()]
genTMFExp c f = mkFun nh sig [p "typ", p "suffix"] rhs (Just bstmts)
  where
    nh = hsTemplateMemberFunctionNameTH c f
    sig = tycon "Type" `tyfun` (tycon "String" `tyfun` (tyapp (tycon "Q") (tycon "Exp")))
    v = mkVar
    p = mkPVar
    tp = tmf_param f
    lit' = strE (hsTemplateMemberFunctionName c f <> "_")
    lam = lambda [p "n"] ( lit' `app` v "<>" `app` v "n")
    rhs = app (v "mkTFunc") (tuple [v "typ", v "suffix", lam, v "tyf"])
    sig' = functionSignatureTMF c f
    bstmts = binds [ mkBind1 "tyf" [mkPVar "n"]
                       (letE [ pbind_ (p tp) (v "pure" `app` (v "typ")) ]
                          (bracketExp (typeBracket sig')))
                       Nothing
                   ]

genTMFInstance :: ClassImportHeader -> TemplateMemberFunction -> [Decl ()]
genTMFInstance cih f = mkFun fname sig [p "isCprim", p "qtyp", p "param"] rhs Nothing
  where
    c = cihClass cih
    fname = "genInstanceFor_" <> hsTemplateMemberFunctionName c f
    p = mkPVar
    v = mkVar
    sig =         tycon "IsCPrimitive"
          `tyfun` (tycon "Q" `tyapp` tycon "Type")
          `tyfun` tycon "TemplateParamInfo"
          `tyfun` (tycon "Q" `tyapp` tylist (tycon "Dec"))
    rhs = doE [suffixstmt, qtypstmt, genstmt, foreignSrcStmt, letStmt lststmt, qualStmt retstmt]
    suffixstmt = letStmt [ pbind_ (p "suffix") (v "tpinfoSuffix" `app` v "param" ) ]
    qtypstmt = generator (p "typ") (v "qtyp")
    genstmt = generator
                (p "f1")
                (v "mkMember" `app` (     strE (hsTemplateMemberFunctionName c f <> "_")
                                    `app` v "<>"
                                    `app` v "suffix"
                                    )
                              `app` v (hsTemplateMemberFunctionNameTH c f)
                              `app` v "typ"
                              `app` v "suffix"
                )
    lststmt = [ pbind_ (p "lst") (list ([v "f1"])) ]
    retstmt = v "pure" `app` v "lst"
    -- TODO: refactor out the following code.
    foreignSrcStmt =
      qualifier $
              (v "addModFinalizer")
        `app` (      v "addForeignSource"
               `app` con "LangCxx"
               `app` (L.foldr1 (\x y -> inapp x (op "++") y)
                        [ includeStatic
                        , includeDynamic
                        , namespaceStr
                        , strE (hsTemplateMemberFunctionName c f)
                        , strE "("
                        , v "suffix"
                        , strE ")\n"
                        ]
                     )
              )
      where
        includeStatic =
          strE $ concatMap ((<>"\n") . R.renderCMacro . R.Include) $
               [ HdrName "MacroPatternMatch.h", cihSelfHeader cih ]
            <> cihIncludedHPkgHeadersInCPP cih
            <> cihIncludedCPkgHeaders cih
        includeDynamic =
          letE
            [ pbind_ (p "headers") (v "tpinfoCxxHeaders" `app` v "param" )
            , pbind_ (pApp (name "f") [p "x"])
                (v "renderCMacro" `app` (con "Include" `app` v "x"))
            ]
            (v "concatMap" `app` v "f" `app` v "headers")
        namespaceStr =
          letE
            [ pbind_ (p "nss") (v "tpinfoCxxNamespaces" `app` v "param" )
            , pbind_ (pApp (name "f") [p "x"])
                (v "renderCStmt" `app` (con "UsingNamespace" `app` v "x"))
            ]
            (v "concatMap" `app` v "f" `app` v "nss")

--------------------
-- Template Class --
--------------------

genTmplInterface :: TemplateClass -> [Decl ()]
genTmplInterface t =
  [ mkData rname [mkTBind tp] [] Nothing
  , mkNewtype hname [mkTBind tp]
      [ qualConDecl Nothing Nothing (conDecl hname [tyapp tyPtr rawtype]) ] Nothing
  , mkClass cxEmpty (typeclassNameT t) [mkTBind tp] methods
  , mkInstance cxEmpty "FPtr" [ hightype ] fptrbody
  , mkInstance cxEmpty "Castable" [ hightype, tyapp tyPtr rawtype ] castBody
  ]
 where (hname,rname) = hsTemplateClassName t
       tp = tclass_param t
       fs = tclass_funcs t
       rawtype = tyapp (tycon rname) (mkTVar tp)
       hightype = tyapp (tycon hname) (mkTVar tp)
       sigdecl f = mkFunSig (hsTmplFuncName t f) (functionSignatureT t f)
       methods = map (clsDecl . sigdecl) fs
       fptrbody = [ insType (tyapp (tycon "Raw") hightype) rawtype
                  , insDecl (mkBind1 "get_fptr" [pApp (name hname) [mkPVar "ptr"]] (mkVar "ptr") Nothing)
                  , insDecl (mkBind1 "cast_fptr_to_obj" [] (con hname) Nothing)
                  ]


genTmplImplementation :: TemplateClass -> [Decl ()]
genTmplImplementation t = concatMap gen (tclass_funcs t)
  where
    gen f = mkFun nh sig [p "typ", p "suffix"] rhs (Just bstmts)
      where nh = hsTmplFuncNameTH t f
            nc = ffiTmplFuncName f
            sig = tycon "Type" `tyfun` (tycon "String" `tyfun` (tyapp (tycon "Q") (tycon "Exp")))
            v = mkVar
            p = mkPVar
            tp = tclass_param t
            prefix = tclass_name t
            lit' = strE (prefix<>"_"<>nc<>"_")
            lam = lambda [p "n"] ( lit' `app` v "<>" `app` v "n")
            rhs = app (v "mkTFunc") (tuple [v "typ", v "suffix", lam, v "tyf"])
            sig' = functionSignatureTT t f
            bstmts = binds [ mkBind1 "tyf" [mkPVar "n"]
                               (letE [ pbind_ (p tp) (v "pure" `app` (v "typ")) ]
                                  (bracketExp (typeBracket sig')))
                               Nothing
                           ]


genTmplInstance ::
     TemplateClassImportHeader
  -> [Decl ()]
genTmplInstance tcih =
    mkFun fname sig [p "isCprim", p "qtyp", p "param"] rhs Nothing
  where t = tcihTClass tcih
        fs = tclass_funcs t
        tname = tclass_name t
        fname = "gen" <> tname <> "InstanceFor"
        p = mkPVar
        v = mkVar
        sig =         tycon "IsCPrimitive"
              `tyfun` (tycon "Q" `tyapp` tycon "Type")
              `tyfun` tycon "TemplateParamInfo"
              `tyfun` (tycon "Q" `tyapp` tylist (tycon "Dec"))
        nfs = zip ([1..] :: [Int]) fs
        rhs = doE (   [ suffixstmt, qtypstmt ]
                   <> map genstmt nfs
                   <> [foreignSrcStmt, letStmt (lststmt nfs), qualStmt retstmt]
                  )
        suffixstmt = letStmt [ pbind_ (p "suffix") (v "tpinfoSuffix" `app` v "param" ) ]
        qtypstmt = generator (p "typ") (v "qtyp")
        genstmt (n,f@TFun    {..}) = generator
                                       (p ("f"<>show n))
                                       (v "mkMember" `app` strE (hsTmplFuncName t f)
                                                     `app` v    (hsTmplFuncNameTH t f)
                                                     `app` v    "typ"
                                                     `app` v    "suffix"
                                       )
        genstmt (n,f@TFunNew {..}) = generator
                                       (p ("f"<>show n))
                                       (v "mkNew"    `app` strE (hsTmplFuncName t f)
                                                     `app` v    (hsTmplFuncNameTH t f)
                                                     `app` v    "typ"
                                                     `app` v    "suffix"
                                       )
        genstmt (n,f@TFunDelete)   = generator
                                       (p ("f"<>show n))
                                       (v "mkDelete" `app` strE (hsTmplFuncName t f)
                                                     `app` v    (hsTmplFuncNameTH t f)
                                                     `app` v    "typ"
                                                     `app` v    "suffix"
                                       )
        lststmt xs = [ pbind_ (p "lst") (list (map (v . (\n->"f"<>show n) . fst) xs)) ]
        -- TODO: refactor out the following code.
        foreignSrcStmt =
          qualifier $
                  (v "addModFinalizer")
            `app` (      v "addForeignSource"
                   `app` con "LangCxx"
                   `app` (L.foldr1 (\x y -> inapp x (op "++") y)
                            [ includeStatic
                            , includeDynamic
                            , namespaceStr
                            , strE (tname <> "_instance")
                            , paren $
                                caseE
                                  (v "isCprim")
                                  [ match (p "CPrim")    (strE "_s")
                                  , match (p "NonCPrim") (strE "")
                                  ]
                            , strE "("
                            , v "suffix"
                            , strE ")\n"
                            ]
                         )
                  )
          where
            includeStatic =
              strE $ concatMap (<> "\n")
                [ R.renderCMacro (R.Include (HdrName "MacroPatternMatch.h"))
                , R.renderCMacro (R.Include (tcihSelfHeader tcih))
                ]
            includeDynamic =
              letE
                [ pbind_ (p "headers") (v "tpinfoCxxHeaders" `app` v "param" )
                , pbind_ (pApp (name "f") [p "x"])
                    (v "renderCMacro" `app` (con "Include" `app` v "x"))
                ]
                (v "concatMap" `app` v "f" `app` v "headers")
            namespaceStr =
              letE
                [ pbind_ (p "nss") (v "tpinfoCxxNamespaces" `app` v "param" )
                , pbind_ (pApp (name "f") [p "x"])
                    (v "renderCStmt" `app` (con "UsingNamespace" `app` v "x"))
                ]
                (v "concatMap" `app` v "f" `app` v "nss")


        retstmt = v "pure"
                  `app` list [ v "mkInstance"
                               `app` list []
                               `app` (con "AppT"
                                      `app` (v "con" `app` strE (typeclassNameT t))
                                      `app` (v "typ")
                                     )
                               `app` (v "lst")
                             ]
