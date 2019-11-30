{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Code.HsProxy where

import Language.Haskell.Exts.Build    ( app, doE, qualStmt, strE )
import qualified Data.List as L       ( foldr1 )
import Language.Haskell.Exts.Syntax   ( Decl(..) )
--
import qualified FFICXX.Runtime.CodeGen.Cxx as R
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
                         )
                  )
          where
            includeStatic =
              strE $ concatMap (<> "\n")
                [ R.renderCMacro (R.Include "MacroPatternMatch.h") ]
        retstmt = v "pure"
                  `app` list []
