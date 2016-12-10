-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Util.HaskellSrcExts
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------


module FFICXX.Generate.Util.HaskellSrcExts where

import           Language.Haskell.Exts
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

unqual = UnQual . Ident

tycon = TyCon . unqual

mkVar = Var . unqual

mkTVar = TyVar . Ident

mkBind1 name pat rhs mbinds =
  FunBind [ Match noLoc (Ident name) pat Nothing (UnGuardedRhs rhs) mbinds ]

mkFunGen :: String -> Type -> [Pat] -> Exp -> Maybe Binds -> [Decl]
mkFunGen funname typ pats rhs mbinds = [sig,defn]
  where sig  = TypeSig noLoc [Ident funname] typ
        defn = mkBind1 funname pats rhs mbinds


mkClass ctxt name tbinds cdecls =
  ClsDecl (ClassDecl noLoc ctxt (Ident name) tbinds [] cdecls)
