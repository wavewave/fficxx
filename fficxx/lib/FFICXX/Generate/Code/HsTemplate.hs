{-# LANGUAGE RecordWildCards   #-}
module FFICXX.Generate.Code.HsTemplate where

import Data.Monoid                             ((<>))
import Language.Haskell.Exts.Build             (app,binds,doE,letE,letStmt,name,pApp
                                               ,qualStmt,strE,tuple)
import Language.Haskell.Exts.Syntax            (Decl(..))
--
import FFICXX.Generate.Code.Primitive          (functionSignatureT
                                               ,functionSignatureTT
                                               ,functionSignatureTMF)
import FFICXX.Generate.Code.HsCast             (castBody)
import FFICXX.Generate.Name                    (ffiTmplFuncName
                                               ,hsTemplateClassName
                                               ,hsTemplateMemberFunctionName
                                               ,hsTemplateMemberFunctionNameTH
                                               ,hsTmplFuncName
                                               ,hsTmplFuncNameTH
                                               ,typeclassNameT)
import FFICXX.Generate.Type.Class              (Class(..)
                                               ,TemplateClass(..)
                                               ,TemplateFunction(..)
                                               ,TemplateMemberFunction(..))
import FFICXX.Generate.Util.HaskellSrcExts     (bracketExp
                                               ,con,conDecl,cxEmpty
                                               ,generator
                                               ,clsDecl,insDecl,insType
                                               ,lambda,list
                                               ,mkBind1,mkTBind,mkData,mkNewtype
                                               ,mkFun,mkFunSig,mkClass,mkInstance
                                               ,mkPVar,mkTVar,mkVar
                                               ,pbind,qualConDecl
                                               ,tyapp,tycon,tyfun,tylist,tyPtr
                                               ,typeBracket)



------------------------------
-- Template member function --
------------------------------

genTemplateMemberFunctions :: Class -> [Decl ()]
genTemplateMemberFunctions c =
  concatMap (\f -> genTMFExp c f <> genTMFInstance c f) (class_tmpl_funcs c)


genTMFExp :: Class -> TemplateMemberFunction -> [Decl ()]
genTMFExp c f = mkFun nh sig [p "typ", p "suffix"] rhs (Just bstmts)
      where nh = hsTemplateMemberFunctionNameTH c f
            sig = tycon "Type" `tyfun` (tycon "String" `tyfun` (tyapp (tycon "Q") (tycon "Exp")))
            v = mkVar
            p = mkPVar
            tp = tmf_param f
            lit' = strE (hsTemplateMemberFunctionName c f <> "_")
            lam = lambda [p "n"] ( lit' `app` v "<>" `app` v "n")
            rhs = app (v "mkTFunc") (tuple [v "typ", v "suffix", lam, v "tyf"])
            sig' = functionSignatureTMF c f
            bstmts = binds [ mkBind1 "tyf" [mkPVar "n"]
                               (letE [ pbind (p tp) (v "pure" `app` (v "typ")) Nothing ]
                                  (bracketExp (typeBracket sig')))
                               Nothing
                           ]


genTMFInstance :: Class -> TemplateMemberFunction -> [Decl ()]
genTMFInstance c f = mkFun fname sig [p "qtyp", p "suffix"] rhs Nothing
  where fname = "genInstanceFor_" <> hsTemplateMemberFunctionName c f
        p = mkPVar
        v = mkVar
        sig = (tyapp (tycon "Q") (tycon "Type")) `tyfun`
                (tycon "String" `tyfun`
                  (tyapp (tycon "Q") (tylist (tycon "Dec"))))
        rhs = doE [qtypstmt, genstmt, letStmt lststmt, qualStmt retstmt]
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
        lststmt = [ pbind (p "lst") (list ([v "f1"])) Nothing ]
        retstmt = v "pure" `app` v "lst"

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
                               (letE [ pbind (p tp) (v "pure" `app` (v "typ")) Nothing ]
                                  (bracketExp (typeBracket sig')))
                               Nothing
                           ]


genTmplInstance :: TemplateClass -> [TemplateFunction] -> [Decl ()]
genTmplInstance t fs = mkFun fname sig [p "qtyp", p "suffix"] rhs Nothing
  where tname = tclass_name t
        fname = "gen" <> tname <> "InstanceFor"
        p = mkPVar
        v = mkVar
        sig = (tyapp (tycon "Q") (tycon "Type")) `tyfun`
                (tycon "String" `tyfun`
                  (tyapp (tycon "Q") (tylist (tycon "Dec"))))
        nfs = zip ([1..] :: [Int]) fs
        rhs = doE (  [qtypstmt]
                  <> map genstmt nfs
                  <> [letStmt (lststmt nfs), qualStmt retstmt])
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
        lststmt xs = [ pbind (p "lst") (list (map (v . (\n->"f"<>show n) . fst) xs)) Nothing ]
        retstmt = v "pure"
                  `app` list [ v "mkInstance"
                               `app` list []
                               `app` (con "AppT"
                                      `app` (v "con" `app` strE (typeclassNameT t))
                                      `app` (v "typ")
                                     )
                               `app` (v "lst")
                             ]
