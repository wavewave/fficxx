{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Code.HsProxy where

import qualified Data.List as L (foldr1)
--

{-
import FFICXX.Generate.Util.HaskellSrcExts
  ( app,
    con,
    doE,
    inapp,
    listE,
    mkFun,
    mkVar,
    op,
    qualStmt,
    qualifier,
    strE,
    tyapp,
    tycon,
    tylist,
  )
-}
import FFICXX.Generate.Util.GHCExactPrint
  ( mkFun,
    testTyp,
  )
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import GHC.Hs.Extension
  ( GhcPs,
  )
import Language.Haskell.Syntax.Decls (HsDecl)

genProxyInstance :: [HsDecl GhcPs]
genProxyInstance = mkFun fname testTyp
  where
    fname = "genImplProxy"

{-  mkFun fname sig [] rhs Nothing
    v = mkVar
    sig = tycon "Q" `tyapp` tylist (tycon "Dec")
    rhs = doE [foreignSrcStmt, qualStmt retstmt]
    foreignSrcStmt =
      qualifier $
        (v "addModFinalizer")
          `app` ( v "addForeignSource"
                    `app` con "LangCxx"
                    `app` ( L.foldr1
                              (\x y -> inapp x (op "++") y)
                              [includeStatic]
                          )
                )
      where
        includeStatic =
          strE $
            concatMap
              (<> "\n")
              [R.renderCMacro (R.Include "MacroPatternMatch.h")]
    retstmt = v "pure" `app` listE []
-}
