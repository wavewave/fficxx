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

unqual :: String  -> QName
unqual = UnQual . Ident

tycon :: String -> Type
tycon = TyCon . unqual

conDecl :: String -> [Type] -> ConDecl
conDecl n ys = ConDecl (Ident n) ys

recDecl :: String -> [([Name],Type)] -> ConDecl
recDecl n rs = RecDecl (Ident n) rs

app :: String -> String -> Exp
app x y = App (mkVar x) (mkVar y)

mkVar :: String -> Exp
mkVar = Var . unqual

mkTVar :: String -> Type
mkTVar = TyVar . Ident

mkPVar :: String -> Pat
mkPVar = PVar . Ident

mkPVarSig :: String -> Type -> Pat
mkPVarSig n typ = PatTypeSig noLoc (mkPVar n) typ

pbind :: Pat -> Rhs -> Maybe Binds -> Decl
pbind = PatBind noLoc

mkTBind :: String -> TyVarBind
mkTBind = UnkindedVar . Ident

mkBind1 :: String -> [Pat] -> Exp -> Maybe Binds -> Decl
mkBind1 n pat rhs mbinds =
  FunBind [ Match noLoc (Ident n) pat Nothing (UnGuardedRhs rhs) mbinds ]

mkFun :: String -> Type -> [Pat] -> Exp -> Maybe Binds -> [Decl]
mkFun fname typ pats rhs mbinds = [mkFunSig fname typ, mkBind1 fname pats rhs mbinds]

mkFunSig :: String -> Type -> Decl
mkFunSig fname typ = TypeSig noLoc [Ident fname] typ

mkClass :: Context -> String -> [TyVarBind] -> [ClassDecl] -> Decl
mkClass ctxt n tbinds cdecls = ClassDecl noLoc ctxt (Ident n) tbinds [] cdecls

mkInstance :: Context -> String -> [Type] -> [InstDecl] -> Decl
mkInstance ctxt n typs idecls = InstDecl noLoc Nothing [] ctxt (unqual n) typs idecls

mkData :: String -> [TyVarBind] -> [QualConDecl] -> [Deriving] -> Decl
mkData n tbinds qdecls derivs  = DataDecl noLoc DataType [] (Ident n) tbinds qdecls derivs

mkNewtype :: String -> [TyVarBind] -> [QualConDecl] -> [Deriving] -> Decl
mkNewtype n tbinds qdecls derivs  = DataDecl noLoc NewType [] (Ident n) tbinds qdecls derivs

mkForImpCcall :: String -> String -> Type -> Decl
mkForImpCcall quote n typ = ForImp noLoc CCall (PlaySafe False) quote (Ident n) typ

mkModule :: String -> [ModulePragma] -> [ImportDecl] -> [Decl] -> Module
mkModule n pragmas idecls decls = Module noLoc (ModuleName n) pragmas Nothing Nothing idecls decls

mkModuleE :: String -> [ModulePragma] -> [ExportSpec] -> [ImportDecl] -> [Decl] -> Module
mkModuleE n pragmas exps idecls decls = Module noLoc (ModuleName n) pragmas Nothing (Just exps) idecls decls

mkImport :: String -> ImportDecl
mkImport m = ImportDecl noLoc (ModuleName m) False False False Nothing Nothing Nothing

mkImportExp :: String -> [String] -> ImportDecl
mkImportExp m lst = ImportDecl noLoc (ModuleName m) False False False Nothing Nothing
                        (Just (False,map (IVar . Ident) lst))

mkImportSrc :: String -> ImportDecl                        
mkImportSrc m = ImportDecl noLoc (ModuleName m) False True False Nothing Nothing Nothing

lang :: [String] -> ModulePragma
lang ns = LanguagePragma noLoc (map Ident ns)

dot :: Exp -> Exp -> Exp
x `dot` y = x `App` mkVar "." `App` y

tyPtr :: Type
tyPtr = tycon "Ptr"

tyForeignPtr :: Type
tyForeignPtr = tycon "ForeignPtr"

