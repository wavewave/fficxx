{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Code.HsProxy where

import qualified Data.List as L (foldr1)
import FFICXX.Generate.Util.GHCExactPrint
  ( app,
    con,
    doE,
    inapp,
    listE,
    mkBodyStmt,
    mkFun,
    mkVar,
    op,
    par,
    strE,
    tyapp,
    tycon,
    tylist,
  )
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import GHC.Hs.Extension
  ( GhcPs,
  )
import Language.Haskell.Syntax.Decls (HsDecl)

genProxyInstance :: [HsDecl GhcPs]
genProxyInstance = mkFun fname sig [] rhs Nothing
  where
    fname = "genImplProxy"
    sig = tycon "Q" `tyapp` tylist (tycon "Dec")
    rhs = doE [foreignSrcStmt, retstmt]

    v = mkVar
    retstmt = mkBodyStmt (v "pure" `app` listE [])

    foreignSrcStmt =
      mkBodyStmt $
        (v "addModFinalizer")
          `app` par
            ( v "addForeignSource"
                `app` con "LangCxx"
                `app` par
                  ( L.foldr1
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
