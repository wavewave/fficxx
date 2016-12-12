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

conDecl name ys = ConDecl (Ident name) ys
recDecl name rs = RecDecl (Ident name) rs


app x y = App (mkVar x) (mkVar y)

mkVar = Var . unqual

mkTVar = TyVar . Ident

mkPVar = PVar . Ident

mkPVarSig n typ = PatTypeSig noLoc (mkPVar n) typ

pbind = PatBind noLoc

mkTBind = UnkindedVar . Ident

mkBind1 name pat rhs mbinds =
  FunBind [ Match noLoc (Ident name) pat Nothing (UnGuardedRhs rhs) mbinds ]

mkFun :: String -> Type -> [Pat] -> Exp -> Maybe Binds -> [Decl]
mkFun fname typ pats rhs mbinds = [mkFunSig fname typ, mkBind1 fname pats rhs mbinds]

mkFunSig :: String -> Type -> Decl
mkFunSig fname typ = TypeSig noLoc [Ident fname] typ

mkClass ctxt name tbinds cdecls =
  ClsDecl (ClassDecl noLoc ctxt (Ident name) tbinds [] cdecls)

mkInstance ctxt name typs idecls = InstDecl noLoc Nothing [] ctxt (unqual name) typs idecls

mkData name tbinds qdecls derivs  = DataDecl noLoc DataType [] (Ident name) tbinds qdecls derivs

mkNewtype name tbinds qdecls derivs  = DataDecl noLoc NewType [] (Ident name) tbinds qdecls derivs

mkForImpCcall quote name typ = ForImp noLoc CCall (PlaySafe False) quote (Ident name) typ



x `dot` y = x `App` mkVar "." `App` y

tyPtr = tycon "Ptr"

tyForeignPtr = tycon "ForeignPtr"
