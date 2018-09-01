{-# LANGUAGE RecordWildCards   #-}
module FFICXX.Generate.Code.HsTemplate where

import Data.Monoid                             ((<>))
import Language.Haskell.Exts.Build             (app,binds,doE,letE,letStmt
                                               ,name,pApp
                                               ,qualStmt,strE,tuple)
import Language.Haskell.Exts.Syntax            (Decl(..))
--
import FFICXX.Generate.Code.Primitive          (ffiTmplFuncName
                                               ,functionSignatureT,functionSignatureTT
                                               ,hsTemplateClassName
                                               ,hsTmplFuncName,hsTmplFuncNameTH
                                               ,typeclassNameT)
import FFICXX.Generate.Code.HsCast             (castBody)
import FFICXX.Generate.Type.Class              (TemplateClass(..),TemplateFunction(..))
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


--------------
-- Template --
--------------

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
    gen f = mkFun nh sig [p "nty", p "ncty"] rhs (Just bstmts)
      where nh = hsTmplFuncNameTH t f
            nc = ffiTmplFuncName f
            sig = tycon "Name" `tyfun` (tycon "String" `tyfun` tycon "ExpQ")
            v = mkVar
            p = mkPVar
            tp = tclass_param t
            prefix = tclass_name t
            lit' = strE (prefix<>"_"<>nc<>"_")
            lam = lambda [p "n"] ( lit' `app` v "<>" `app` v "n")
            rhs = app (v "mkTFunc") (tuple [v "nty", v "ncty", lam, v "tyf"])
            sig' = functionSignatureTT t f
            bstmts = binds [ mkBind1 "tyf" [mkPVar "n"]
                               (letE [ pbind (p tp) (v "return" `app` (con "ConT" `app` v "n")) Nothing ]
                                  (bracketExp (typeBracket sig')))
                               Nothing
                           ]


genTmplInstance :: TemplateClass -> [TemplateFunction] -> [Decl ()]
genTmplInstance t fs = mkFun fname sig [p "n", p "ctyp"] rhs Nothing
  where tname = tclass_name t
        fname = "gen" <> tname <> "InstanceFor"
        p = mkPVar
        v = mkVar
        sig = tycon "Name" `tyfun` (tycon "String" `tyfun` (tyapp (tycon "Q") (tylist (tycon "Dec"))))

        nfs = zip ([1..] :: [Int]) fs
        rhs = doE (map genstmt nfs <> [letStmt (lststmt nfs), qualStmt retstmt])

        genstmt (n,f@TFun    {..}) = generator
                                       (p ("f"<>show n))
                                       (v "mkMember" `app` strE (hsTmplFuncName t f)
                                                     `app` v    (hsTmplFuncNameTH t f)
                                                     `app` v    "n"
                                                     `app` v    "ctyp"
                                       )
        genstmt (n,f@TFunNew {..}) = generator
                                       (p ("f"<>show n))
                                       (v "mkNew"    `app` strE (hsTmplFuncName t f)
                                                     `app` v    (hsTmplFuncNameTH t f)
                                                     `app` v    "n"
                                                     `app` v    "ctyp"
                                       )
        genstmt (n,f@TFunDelete)   = generator
                                       (p ("f"<>show n))
                                       (v "mkDelete" `app` strE (hsTmplFuncName t f)
                                                     `app` v    (hsTmplFuncNameTH t f)
                                                     `app` v    "n"
                                                     `app` v    "ctyp"
                                       )
        lststmt xs = [ pbind (p "lst") (list (map (v . (\n->"f"<>show n) . fst) xs)) Nothing ]
        retstmt = v "return"
                  `app` list [ v "mkInstance"
                               `app` list []
                               `app` (con "AppT"
                                      `app` (v "con" `app` strE (typeclassNameT t))
                                      `app` (con "ConT" `app` (v "n"))
                                     )
                               `app` (v "lst")
                             ]
