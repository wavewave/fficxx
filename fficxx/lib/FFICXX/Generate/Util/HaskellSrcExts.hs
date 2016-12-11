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

app x y = App (mkVar x) (mkVar y)

mkVar = Var . unqual

mkTVar = TyVar . Ident

mkPVar = PVar . Ident


mkTBind = UnkindedVar . Ident

mkBind1 name pat rhs mbinds =
  FunBind [ Match noLoc (Ident name) pat Nothing (UnGuardedRhs rhs) mbinds ]

mkFunDecl :: String -> Type -> [Pat] -> Exp -> Maybe Binds -> [Decl]
mkFunDecl fname typ pats rhs mbinds = [mkFunSigDecl fname typ, mkBind1 fname pats rhs mbinds]

mkFunSigDecl :: String -> Type -> Decl
mkFunSigDecl fname typ = TypeSig noLoc [Ident fname] typ

mkClass ctxt name tbinds cdecls =
  ClsDecl (ClassDecl noLoc ctxt (Ident name) tbinds [] cdecls)

mkInstance ctxt name typs idecls = InstDecl noLoc Nothing [] ctxt (unqual name) typs idecls

mkData name tbinds qdecls derivs  = DataDecl noLoc DataType [] (Ident name) tbinds qdecls derivs

mkNewtype name tbinds qdecls derivs  = DataDecl noLoc NewType [] (Ident name) tbinds qdecls derivs


x `dot` y = x `App` mkVar "." `App` y
