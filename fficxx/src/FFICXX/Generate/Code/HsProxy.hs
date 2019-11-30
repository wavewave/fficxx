{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Code.HsProxy where

import Language.Haskell.Exts.Build    ( app, doE, qualStmt, strE )
import qualified Data.List as L       ( foldr1 )
import Language.Haskell.Exts.Syntax   ( Decl(..) )
--
import qualified FFICXX.Runtime.CodeGen.C as R
import FFICXX.Generate.Util.HaskellSrcExts
                                      ( con, inapp, list, mkFun, mkPVar, mkVar
                                      , op, qualifier
                                      , tyapp, tycon, tylist
                                      )


genProxyInstance :: [Decl ()]
genProxyInstance =
    mkFun fname sig [] rhs Nothing
  where fname = "genImplProxy"
        p = mkPVar
        v = mkVar
        sig = tycon "Q" `tyapp` tylist (tycon "Dec")
        rhs = doE [foreignSrcStmt, qualStmt retstmt]
        foreignSrcStmt =
          qualifier $
                  (v "addModFinalizer")
            `app` (      v "addForeignSource"
                   `app` con "LangCxx"
                   `app` (L.foldr1 (\x y -> inapp x (op "++") y)
                            [ includeStatic ]
{-                            , includeDynamic
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
                            ] -}
                         )
                  )
          where
            includeStatic =
              strE $ concatMap (<> "\n")
                [ R.renderCMacro (R.Include "MacroPatternMatch.h") ]
{-
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
-}
        retstmt = v "pure"
                  `app` list []
                  {-v "mkInstance"
                               `app` list []
                               `app` (con "AppT"
                                      `app` (v "con" `app` strE (typeclassNameT t))
                                      `app` (v "typ")
                                     )
                               `app` (v "lst")
                             ]

-}
