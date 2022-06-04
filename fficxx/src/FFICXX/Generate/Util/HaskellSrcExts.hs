module FFICXX.Generate.Util.HaskellSrcExts where

import Data.List (foldl')
import Data.Maybe (maybeToList)
import Language.Haskell.Exts
  ( Alt (..),
    Asst (TypeA),
    Binds,
    Bracket (TypeBracket),
    CallConv (CCall),
    ClassDecl (ClsDecl),
    ConDecl
      ( ConDecl,
        RecDecl
      ),
    Context
      ( CxEmpty,
        CxTuple
      ),
    DataOrNew
      ( DataType,
        NewType
      ),
    Decl
      ( ClassDecl,
        DataDecl,
        ForImp,
        FunBind,
        InstDecl,
        PatBind,
        TypeSig
      ),
    DeclHead
      ( DHApp,
        DHead
      ),
    Deriving (..),
    EWildcard (..),
    Exp
      ( App,
        BracketExp,
        Con,
        If,
        InfixApp,
        Lit,
        Var
      ),
    ExportSpec
      ( EAbs,
        EModuleContents,
        EThingWith,
        EVar
      ),
    ExportSpecList (..),
    FieldDecl,
    ImportDecl (..),
    ImportSpec (IVar),
    ImportSpecList (..),
    InstDecl
      ( InsDecl,
        InsType
      ),
    InstHead
      ( IHApp,
        IHCon
      ),
    InstRule (IRule),
    Literal,
    Match (..),
    Module (..),
    ModuleHead (..),
    ModuleName (..),
    ModulePragma (LanguagePragma),
    Name
      ( Ident,
        Symbol
      ),
    Namespace (NoNamespace),
    Pat
      ( PVar,
        PatTypeSig
      ),
    QName (UnQual),
    QOp (QVarOp),
    QualConDecl (..),
    Rhs (UnGuardedRhs),
    Safety (PlaySafe),
    Splice (ParenSplice),
    Stmt
      ( Generator,
        Qualifier
      ),
    TyVarBind (UnkindedVar),
    Type
      ( TyApp,
        TyCon,
        TyForall,
        TyFun,
        TyList,
        TyParen,
        TySplice,
        TyVar
      ),
    app,
    unit_tycon,
  )
import Language.Haskell.Exts.Syntax (CName)

unqual :: String -> QName ()
unqual = UnQual () . Ident ()

tycon :: String -> Type ()
tycon = TyCon () . unqual

tyapp :: Type () -> Type () -> Type ()
tyapp = TyApp ()

infixl 2 `tyapp`

tyfun :: Type () -> Type () -> Type ()
tyfun = TyFun ()

infixr 2 `tyfun`

tylist :: Type () -> Type ()
tylist = TyList ()

unit_tycon :: Type ()
unit_tycon = Language.Haskell.Exts.unit_tycon ()

conDecl :: String -> [Type ()] -> ConDecl ()
conDecl n ys = ConDecl () (Ident () n) ys

qualConDecl ::
  Maybe [TyVarBind ()] ->
  Maybe (Context ()) ->
  ConDecl () ->
  QualConDecl ()
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

pbind_ :: Pat () -> Exp () -> Decl ()
pbind_ p e = pbind p e Nothing

mkTBind :: String -> TyVarBind ()
mkTBind = UnkindedVar () . Ident ()

mkBind1 :: String -> [Pat ()] -> Exp () -> Maybe (Binds ()) -> Decl ()
mkBind1 n pat rhs mbinds =
  FunBind () [Match () (Ident () n) pat (UnGuardedRhs () rhs) mbinds]

mkFun :: String -> Type () -> [Pat ()] -> Exp () -> Maybe (Binds ()) -> [Decl ()]
mkFun fname typ pats rhs mbinds = [mkFunSig fname typ, mkBind1 fname pats rhs mbinds]

mkFunSig :: String -> Type () -> Decl ()
mkFunSig fname typ = TypeSig () [Ident () fname] typ

mkClass :: Context () -> String -> [TyVarBind ()] -> [ClassDecl ()] -> Decl ()
mkClass ctxt n tbinds cdecls = ClassDecl () (Just ctxt) (mkDeclHead n tbinds) [] (Just cdecls)

dhead :: String -> DeclHead ()
dhead n = DHead () (Ident () n)

mkDeclHead :: String -> [TyVarBind ()] -> DeclHead ()
mkDeclHead n tbinds = foldl' (DHApp ()) (dhead n) tbinds

mkInstance :: Context () -> String -> [Type ()] -> [InstDecl ()] -> Decl ()
mkInstance ctxt n typs idecls = InstDecl () Nothing instrule (Just idecls)
  where
    instrule = IRule () Nothing (Just ctxt) insthead
    insthead = foldl' f (IHCon () (unqual n)) typs
      where
        f acc x = IHApp () acc (tyParen x)

mkData :: String -> [TyVarBind ()] -> [QualConDecl ()] -> Maybe (Deriving ()) -> Decl ()
mkData n tbinds qdecls mderiv = DataDecl () (DataType ()) Nothing declhead qdecls (maybeToList mderiv)
  where
    declhead = mkDeclHead n tbinds

mkNewtype :: String -> [TyVarBind ()] -> [QualConDecl ()] -> Maybe (Deriving ()) -> Decl ()
mkNewtype n tbinds qdecls mderiv = DataDecl () (NewType ()) Nothing declhead qdecls (maybeToList mderiv)
  where
    declhead = mkDeclHead n tbinds

mkForImpCcall :: String -> String -> Type () -> Decl ()
mkForImpCcall quote n typ = ForImp () (CCall ()) (Just (PlaySafe () False)) (Just quote) (Ident () n) typ

mkModule :: String -> [ModulePragma ()] -> [ImportDecl ()] -> [Decl ()] -> Module ()
mkModule n pragmas idecls decls = Module () (Just mhead) pragmas idecls decls
  where
    mhead = ModuleHead () (ModuleName () n) Nothing Nothing

mkModuleE :: String -> [ModulePragma ()] -> [ExportSpec ()] -> [ImportDecl ()] -> [Decl ()] -> Module ()
mkModuleE n pragmas exps idecls decls = Module () (Just mhead) pragmas idecls decls
  where
    mhead = ModuleHead () (ModuleName () n) Nothing (Just eslist)
    eslist = ExportSpecList () exps

mkImport :: String -> ImportDecl ()
mkImport m = ImportDecl () (ModuleName () m) False False False Nothing Nothing Nothing

mkImportExp :: String -> [String] -> ImportDecl ()
mkImportExp m lst =
  ImportDecl () (ModuleName () m) False False False Nothing Nothing (Just islist)
  where
    islist = ImportSpecList () False (map mkIVar lst)

mkImportSrc :: String -> ImportDecl ()
mkImportSrc m = ImportDecl () (ModuleName () m) False True False Nothing Nothing Nothing

lang :: [String] -> ModulePragma ()
lang ns = LanguagePragma () (map (Ident ()) ns)

dot :: Exp () -> Exp () -> Exp ()
x `dot` y = x `app` mkVar "." `app` y

tyForall ::
  Maybe [TyVarBind ()] ->
  Maybe (Context ()) ->
  Type () ->
  Type ()
tyForall = TyForall ()

tyParen :: Type () -> Type ()
tyParen = TyParen ()

tyPtr :: Type ()
tyPtr = tycon "Ptr"

tyForeignPtr :: Type ()
tyForeignPtr = tycon "ForeignPtr"

classA :: QName () -> [Type ()] -> Asst ()
classA n = TypeA () . foldl' tyapp (TyCon () n)

cxEmpty :: Context ()
cxEmpty = CxEmpty ()

cxTuple :: [Asst ()] -> Context ()
cxTuple = CxTuple ()

tySplice :: Splice () -> Type ()
tySplice = TySplice ()

parenSplice :: Exp () -> Splice ()
parenSplice = ParenSplice ()

bracketExp :: Bracket () -> Exp ()
bracketExp = BracketExp ()

typeBracket :: Type () -> Bracket ()
typeBracket = TypeBracket ()

mkDeriving :: [InstRule ()] -> Deriving ()
mkDeriving = Deriving () Nothing

irule ::
  Maybe [TyVarBind ()] ->
  Maybe (Context ()) ->
  InstHead () ->
  InstRule ()
irule = IRule ()

ihcon :: QName () -> InstHead ()
ihcon = IHCon ()

evar :: QName () -> ExportSpec ()
evar = EVar ()

eabs :: Namespace () -> QName () -> ExportSpec ()
eabs = EAbs ()

ethingwith ::
  EWildcard () ->
  QName () ->
  [Language.Haskell.Exts.Syntax.CName ()] ->
  ExportSpec ()
ethingwith = EThingWith ()

ethingall :: QName () -> ExportSpec ()
ethingall q = ethingwith (EWildcard () 0) q []

emodule :: String -> ExportSpec ()
emodule nm = EModuleContents () (ModuleName () nm)

nonamespace :: Namespace ()
nonamespace = NoNamespace ()

insType :: Type () -> Type () -> InstDecl ()
insType = InsType ()

insDecl :: Decl () -> InstDecl ()
insDecl = InsDecl ()

generator :: Pat () -> Exp () -> Stmt ()
generator = Generator ()

qualifier :: Exp () -> Stmt ()
qualifier = Qualifier ()

clsDecl :: Decl () -> ClassDecl ()
clsDecl = ClsDecl ()

unkindedVar :: Name () -> TyVarBind ()
unkindedVar = UnkindedVar ()

op :: String -> QOp ()
op = QVarOp () . UnQual () . Symbol ()

inapp :: Exp () -> QOp () -> Exp () -> Exp ()
inapp = InfixApp ()

if_ :: Exp () -> Exp () -> Exp () -> Exp ()
if_ = If ()

urhs :: Exp () -> Rhs ()
urhs = UnGuardedRhs ()

-- | case pattern match p -> e
match :: Pat () -> Exp () -> Alt ()
match p e = Alt () p (urhs e) Nothing
