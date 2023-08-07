{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Util.GHCExactPrint
  ( -- * module
    mkModule,
    -- * decls
    mkImport,
    {- app,
    app',
    unqual,
    tycon,
    tyapp,
    tyfun,
    tylist,
    unit_tycon,
    conDecl,
    qualConDecl,
    recDecl,
    lit,
    mkVar,
    con,
    doE,
    listE,
    strE,
    qualStmt,
    mkTVar,
    mkPVar,
    mkIVar,
    mkPVarSig,
    pbind,
    pbind_,
    mkTBind,
    mkBind1,
    mkFun,
    mkFunSig,
    mkClass,
    dhead,
    mkDeclHead,
    mkInstance,
    mkData,
    mkNewtype,
    mkForImpCcall,
    mkModuleE,
    mkImportExp,
    mkImportSrc,
    lang,
    dot,
    tyForall,
    tyParen,
    tyPtr,
    tyForeignPtr,
    classA,
    cxEmpty,
    cxTuple,
    tySplice,
    tyTupleBoxed,
    parenSplice,
    bracketExp,
    typeBracket,
    mkDeriving,
    irule,
    ihcon,
    evar,
    eabs,
    ethingwith,
    ethingall,
    emodule,
    nonamespace,
    insType,
    insDecl,
    generator,
    qualifier,
    clsDecl,
    unkindedVar,
    op,
    inapp,
    if_,
    urhs,
    match,
    eWildCard, -}

    -- * utility
    exactPrint,
  )
where

import Data.List (foldl')
import Data.Maybe (maybeToList)
import Data.String (IsString (fromString))
import GHC.Hs
  ( AnnsModule (..),
    XModulePs (..),
  )
import GHC.Hs.Extension
  ( GhcPs,
  )
import GHC.Hs.ImpExp
  ( XImportDeclPass (..),
  )
import GHC.Parser.Annotation
  ( AddEpAnn (..),
    Anchor (..),
    AnchorOperation (..),
    AnnKeywordId (..),
    AnnList (..),
    AnnListItem (..),
    DeltaPos (..),
    EpAnn (..),
    EpaComment (..),
    EpaCommentTok (EpaLineComment),
    EpaLocation (..),
    SrcAnn,
    SrcSpanAnn' (SrcSpanAnn),
    emptyComments,
    noAnn,
    noSrcSpanA,
    spanAsAnchor,
  )
import GHC.Types.PkgQual
  ( RawPkgQual (..),
  )
import GHC.Types.SourceText
  ( SourceText (..),
  )
import GHC.Types.SrcLoc
  ( GenLocated (L),
    RealSrcSpan,
    SrcSpan (..),
    mkSrcLoc,
    mkSrcSpan,
    srcLocSpan,
  )
import qualified Language.Haskell.GHC.ExactPrint as Exact
import Language.Haskell.Syntax
  ( HsModule (..),
    ImportDecl (..),
    LayoutInfo (..),
    ModuleName (..),
  )
import Language.Haskell.Syntax.ImpExp
  ( ImportDeclQualifiedStyle (..),
    IsBootInterface (..),
  )

mkRelAnchor :: Bool -> SrcSpan -> Anchor
mkRelAnchor isSameLine spn =
  let a' = spanAsAnchor spn
   in if isSameLine
        then a' {anchor_op = MovedAnchor (SameLine 1)}
        else a' {anchor_op = MovedAnchor (DifferentLine 1 0)}

mkRelSrcSpanAnn :: Bool -> SrcSpan -> ann -> SrcAnn ann
mkRelSrcSpanAnn isSameLine spn ann =
  SrcSpanAnn (EpAnn (mkRelAnchor isSameLine spn) ann emptyComments) spn

defSrcSpan :: (SrcSpan, RealSrcSpan)
defSrcSpan = (spn, rspn)
  where
    sloc = mkSrcLoc "test" 1 1
    spn = srcLocSpan sloc
    RealSrcSpan rspn _ = spn

mkModule ::
  -- | Module name
  String ->
  -- | Pragmas
  [String] ->
  [ImportDecl GhcPs] ->
  -- [Decl ()] ->
  HsModule GhcPs
mkModule name pragmas idecls {- decls -} =
  HsModule
    { hsmodExt =
        XModulePs
          { hsmodAnn = EpAnn (spanAsAnchor s1) a1 emptyComments,
            hsmodLayout = VirtualBraces 1,
            hsmodDeprecMessage = Nothing,
            hsmodHaddockModHeader = Nothing
          },
      hsmodName = Just (L (mkRelSrcSpanAnn True s1 (AnnListItem [])) modName),
      hsmodExports = Nothing,
      hsmodImports =
        fmap (\idecl -> L (mkRelSrcSpanAnn False s1 (AnnListItem [])) idecl) idecls,
      hsmodDecls = []
    }
  where
    (s1, rs1) = defSrcSpan
    modName = ModuleName (fromString name)
    pragmaComments =
      let ls =
            fmap
              (\p ->
                 let a = mkRelAnchor False s1
                     str = "{-# LANGUAGE " <> p <> " #-}"
                     c = EpaComment (EpaLineComment str) rs1
                  in L a c
              )
              pragmas
       in ls

    a1 =
      AnnsModule
        [ AddEpAnn AnnModule (EpaDelta (DifferentLine 2 0) pragmaComments),
          AddEpAnn AnnWhere (EpaDelta (SameLine 1) [])
        ]
        (AnnList Nothing Nothing Nothing [] [])

mkImport ::
  -- | Module name
  String ->
  ImportDecl GhcPs
mkImport name =
  ImportDecl
    { ideclExt = XImportDeclPass noAnn NoSourceText False,
      ideclName = L (mkRelSrcSpanAnn True s1 (AnnListItem [])) modName,
      ideclPkgQual = NoRawPkgQual,
      ideclSource = NotBoot,
      ideclSafe = False,
      ideclQualified = NotQualified,
      ideclAs = Nothing,
      ideclImportList = Nothing
    }
  where
    (s1, rs1) = defSrcSpan
    modName = ModuleName (fromString name)


--

--
-- utilities
--

-- | exact print
exactPrint :: (Exact.ExactPrint ast) => ast -> String
exactPrint = Exact.exactPrint . Exact.makeDeltaAst

{-
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
    Safety (PlayInterruptible),
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
        TyTuple,
        TyVar
      ),
  ) -}
-- import qualified Language.Haskell.Exts as LHE
-- import Language.Haskell.Exts.Syntax (CName)

{-
app :: Exp () -> Exp () -> Exp ()
app = LHE.app

app' :: String -> String -> Exp ()
app' x y = App () (mkVar x) (mkVar y)

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
unit_tycon = LHE.unit_tycon ()

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

lit :: Literal () -> Exp ()
lit = Lit ()

mkVar :: String -> Exp ()
mkVar = Var () . unqual

con :: String -> Exp ()
con = Con () . unqual

doE :: [Stmt ()] -> Exp ()
doE = LHE.doE

listE :: [Exp ()] -> Exp ()
listE = LHE.listE

strE :: String -> Exp ()
strE = LHE.strE

qualStmt :: Exp () -> Stmt ()
qualStmt = LHE.qualStmt

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
mkForImpCcall quote n typ = ForImp () (CCall ()) (Just (PlayInterruptible ())) (Just quote) (Ident () n) typ

mkModuleE :: String -> [ModulePragma ()] -> [ExportSpec ()] -> [ImportDecl ()] -> [Decl ()] -> Module ()
mkModuleE n pragmas exps idecls decls = Module () (Just mhead) pragmas idecls decls
  where
    mhead = ModuleHead () (ModuleName () n) Nothing (Just eslist)
    eslist = ExportSpecList () exps


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

tyTupleBoxed :: [Type ()] -> Type ()
tyTupleBoxed = TyTuple () LHE.Boxed

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

eWildCard :: Int -> EWildcard ()
eWildCard = EWildcard ()
-}
