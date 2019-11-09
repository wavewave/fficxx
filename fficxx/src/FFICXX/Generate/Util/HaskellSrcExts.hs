{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Util.HaskellSrcExts
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------


module FFICXX.Generate.Util.HaskellSrcExts where

import           Data.Maybe                          (maybeToList)
import           Data.List                           (foldl')
import           Language.Haskell.Exts        hiding (unit_tycon)
import qualified Language.Haskell.Exts               (unit_tycon)


unqual :: String  -> QName ()
unqual = UnQual () . Ident ()

tycon :: String -> Type ()
tycon = TyCon () . unqual

tyapp :: Type () -> Type () -> Type ()
tyapp = TyApp ()

tyfun :: Type () -> Type () -> Type ()
tyfun = TyFun ()

tylist :: Type () -> Type ()
tylist = TyList ()

unit_tycon :: Type ()
unit_tycon = Language.Haskell.Exts.unit_tycon ()

conDecl :: String -> [Type ()] -> ConDecl ()
conDecl n ys = ConDecl () (Ident () n) ys

qualConDecl = QualConDecl ()

recDecl :: String -> [FieldDecl ()] -> ConDecl ()
recDecl n rs = RecDecl () (Ident () n) rs

app' :: String -> String -> Exp ()
app' x y = App () (mkVar x) (mkVar y)


lit :: Literal () -> Exp ()
lit = Lit ()


mkVar :: String -> Exp ()
mkVar = Var () . unqual

con :: String -> Exp ()
con = Con () . unqual

mkTVar :: String -> Type ()
mkTVar = TyVar () . Ident ()

mkPVar :: String -> Pat ()
mkPVar = PVar () . Ident ()

mkIVar :: String -> ImportSpec ()
mkIVar = IVar () . Ident ()

mkPVarSig :: String -> Type () -> Pat ()
mkPVarSig n typ = PatTypeSig () (mkPVar n) typ

pbind :: Pat () -> Exp () -> Maybe (Binds ()) -> Decl ()
pbind pat e = PatBind () pat (UnGuardedRhs () e)

mkTBind :: String -> TyVarBind ()
mkTBind = UnkindedVar () . Ident ()

mkBind1 :: String -> [Pat ()] -> Exp () -> Maybe (Binds ()) -> Decl ()
mkBind1 n pat rhs mbinds =
  FunBind () [ Match () (Ident () n) pat (UnGuardedRhs () rhs) mbinds ]

mkFun :: String -> Type () -> [Pat ()] -> Exp () -> Maybe (Binds ()) -> [Decl ()]
mkFun fname typ pats rhs mbinds = [mkFunSig fname typ, mkBind1 fname pats rhs mbinds]

mkFunSig :: String -> Type () -> Decl ()
mkFunSig fname typ = TypeSig () [Ident () fname] typ

mkClass :: Context () -> String -> [TyVarBind ()] -> [ClassDecl ()] -> Decl ()
mkClass ctxt n tbinds cdecls = ClassDecl () (Just ctxt) (mkDeclHead n tbinds)  [] (Just cdecls)


dhead :: String -> DeclHead ()
dhead n = DHead () (Ident () n)

mkDeclHead :: String -> [TyVarBind ()] -> DeclHead ()
mkDeclHead n tbinds = foldl' (DHApp ()) (dhead n) tbinds

mkInstance :: Context () -> String -> [Type ()] -> [InstDecl ()] -> Decl ()
mkInstance ctxt n typs idecls = InstDecl () Nothing instrule (Just idecls)
  where instrule = IRule () Nothing (Just ctxt) insthead
        insthead  = foldl' f (IHCon () (unqual n)) typs
          where f acc x = IHApp () acc (tyParen x)

mkData :: String -> [TyVarBind ()] -> [QualConDecl ()] -> Maybe (Deriving ()) -> Decl ()
#if MIN_VERSION_haskell_src_exts(1,20,0)
mkData n tbinds qdecls mderiv  = DataDecl () (DataType ()) Nothing declhead qdecls (maybeToList mderiv)
#else
mkData n tbinds qdecls mderiv  = DataDecl () (DataType ()) Nothing declhead qdecls mderiv
#endif
  where declhead = mkDeclHead n tbinds

mkNewtype :: String -> [TyVarBind ()] -> [QualConDecl ()] -> Maybe (Deriving ()) -> Decl ()
#if MIN_VERSION_haskell_src_exts(1,20,0)
mkNewtype n tbinds qdecls mderiv  = DataDecl () (NewType ()) Nothing declhead qdecls (maybeToList mderiv)
#else
mkNewtype n tbinds qdecls mderiv  = DataDecl () (NewType ()) Nothing declhead qdecls mderiv
#endif
  where declhead = mkDeclHead n tbinds

mkForImpCcall :: String -> String -> Type () -> Decl ()
mkForImpCcall quote n typ = ForImp () (CCall ()) (Just (PlaySafe () False)) (Just quote) (Ident () n) typ

mkModule :: String -> [ModulePragma ()] -> [ImportDecl ()] -> [Decl ()] -> Module ()
mkModule n pragmas idecls decls = Module () (Just mhead) pragmas idecls decls
  where mhead = ModuleHead () (ModuleName () n) Nothing Nothing

mkModuleE :: String -> [ModulePragma ()] -> [ExportSpec ()] -> [ImportDecl ()] -> [Decl ()] -> Module ()
mkModuleE n pragmas exps idecls decls = Module () (Just mhead) pragmas  idecls decls
  where mhead = ModuleHead () (ModuleName () n) Nothing (Just eslist)
        eslist = ExportSpecList () exps

mkImport :: String -> ImportDecl ()
mkImport m = ImportDecl () (ModuleName () m) False False False Nothing Nothing Nothing

mkImportExp :: String -> [String] -> ImportDecl ()
mkImportExp m lst =
  ImportDecl () (ModuleName () m) False False False Nothing Nothing (Just islist)
  where islist = ImportSpecList () False (map mkIVar lst)


mkImportSrc :: String -> ImportDecl ()
mkImportSrc m = ImportDecl () (ModuleName () m) False True False Nothing Nothing Nothing

lang :: [String] -> ModulePragma ()
lang ns = LanguagePragma () (map (Ident ()) ns)

dot :: Exp () -> Exp () -> Exp ()
x `dot` y = x `app` mkVar "." `app` y

tyForall = TyForall ()

tyParen = TyParen ()

tyPtr :: Type ()
tyPtr = tycon "Ptr"

tyForeignPtr :: Type ()
tyForeignPtr = tycon "ForeignPtr"

classA :: QName () -> [Type ()] -> Asst ()
classA = ClassA ()

cxEmpty :: Context ()
cxEmpty = CxEmpty ()

cxTuple :: [Asst ()] -> Context ()
cxTuple = CxTuple ()

tySplice :: Splice () -> Type ()
tySplice = TySplice ()

parenSplice :: Exp () -> Splice ()
parenSplice = ParenSplice ()

bracketExp = BracketExp ()
typeBracket = TypeBracket ()


#if MIN_VERSION_haskell_src_exts(1,20,0)
mkDeriving = Deriving () Nothing
#else
mkDeriving = Deriving ()
#endif

irule = IRule ()

ihcon = IHCon ()

evar = EVar ()
eabs = EAbs ()
ethingwith = EThingWith ()

ethingall q = ethingwith (EWildcard () 0) q []

emodule nm = EModuleContents () (ModuleName () nm)

nonamespace = NoNamespace ()

list = List ()
lambda = Lambda ()

insType = InsType ()
insDecl = InsDecl ()

generator = Generator ()

qualifier = Qualifier ()

clsDecl = ClsDecl ()


unkindedVar = UnkindedVar ()

op = QVarOp () . UnQual () . Symbol ()

inapp = InfixApp ()

if_ = If ()

litString x x' = lit (String () x x')
