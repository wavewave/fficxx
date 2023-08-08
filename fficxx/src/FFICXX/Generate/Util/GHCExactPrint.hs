{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Util.GHCExactPrint
  ( -- * module
    mkModule,

    -- * import
    mkImport,

    -- * names
    unqual,

    -- * types
    mkTVar,
    tycon,
    tyapp,
    tylist,

    -- * function
    mkFun,
    mkFunSig,
    mkBind1,

    -- * expr
    app,
    doE,
    listE,
    mkVar,

    -- * stmt
    mkBodyStmt,

    {- app',
    tyfun,
    unit_tycon,
    conDecl,
    qualConDecl,
    recDecl,
    lit,
    con,
    strE,
    qualStmt,
    mkTVar,
    mkPVar,
    mkIVar,
    mkPVarSig,
    pbind,
    pbind_,
    mkTBind,
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
import GHC.Hs.Binds
  ( AnnSig (..),
  )
import GHC.Hs
  ( GrhsAnn (..),
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
    AnnParen (..),
    DeltaPos (..),
    EpAnn (..),
    EpaComment (..),
    EpaCommentTok (EpaLineComment),
    EpaLocation (..),
    NameAnn (..),
    NoEpAnns (..),
    ParenType (AnnParensSquare),
    SrcAnn,
    SrcSpanAnn' (SrcSpanAnn),
    SrcSpanAnnA,
    emptyComments,
    noAnn,
    noSrcSpanA,
    spanAsAnchor,
  )
import GHC.Types.Basic
  ( Origin (FromSource),
  )
import GHC.Types.Fixity
  ( LexicalFixity (Prefix),
  )
import GHC.Types.Name.Occurrence
  ( OccName,
    mkOccName,
    mkTyVarOcc,
    mkVarOcc,
  )
import GHC.Types.Name.Reader
  ( RdrName (Unqual),
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
  ( Anno,
    ExprLStmt,
    GRHS (..),
    GRHSs (..),
    HsBind (..),
    HsBindLR (..),
    HsDecl (..),
    HsDoFlavour (..),
    HsExpr (..),
    HsLocalBinds,
    HsLocalBindsLR (..),
    HsMatchContext (FunRhs),
    HsModule (..),
    HsOuterTyVarBndrs (HsOuterImplicit),
    HsSigType (HsSig),
    HsType (..),
    HsWildCardBndrs (HsWC),
    ImportDecl (..),
    ImportDeclQualifiedStyle (..),
    IsBootInterface (..),
    LayoutInfo (..),
    LHsExpr,
    Match (..),
    MatchGroup (..),
    ModuleName (..),
    Pat (..),
    PromotionFlag (..),
    Sig (TypeSig),
    StmtLR (..),
    noExtField,
  )
import Language.Haskell.Syntax.Basic
  ( SrcStrictness (NoSrcStrict),
  )

mkRelAnchor :: Int -> SrcSpan -> Anchor
mkRelAnchor nLines spn =
  let a' = spanAsAnchor spn
   in if
          | nLines < -1 -> error "mkRelAnchor: cannot go backward further"
          | nLines == -1 -> a' {anchor_op = MovedAnchor (SameLine 0)}
          | nLines == 0 -> a' {anchor_op = MovedAnchor (SameLine 1)}
          | nLines > 0 -> a' {anchor_op = MovedAnchor (DifferentLine nLines 0)}

mkRelEpAnn :: Int -> SrcSpan -> ann -> EpAnn ann
mkRelEpAnn nLines spn ann = EpAnn (mkRelAnchor nLines spn) ann emptyComments

mkRelSrcSpanAnn :: Int -> SrcSpan -> ann -> SrcAnn ann
mkRelSrcSpanAnn nLines spn ann =
  SrcSpanAnn (mkRelEpAnn nLines spn ann) spn

defSrcSpan :: (SrcSpan, RealSrcSpan)
defSrcSpan = (spn, rspn)
  where
    sloc = mkSrcLoc "test" 1 1
    spn = srcLocSpan sloc
    RealSrcSpan rspn _ = spn

paragraphLines :: SrcSpan -> [a] -> [GenLocated SrcSpanAnnA a]
paragraphLines spn zs =
  case zs of
    x : xs ->
      let x' = L (mkRelSrcSpanAnn 2 spn (AnnListItem [])) x
          xs' = fmap (L (mkRelSrcSpanAnn 1 spn (AnnListItem []))) xs
       in x' : xs'
    [] -> []

-- | can place the group of items with arbitrary indentation.
paragraphLines' :: SrcSpan -> DeltaPos -> [a] -> [GenLocated SrcSpanAnnA a]
paragraphLines' spn delta zs =
  case zs of
    x : xs ->
      let a = spanAsAnchor spn
          a' = a {anchor_op = MovedAnchor delta}
          ann' = SrcSpanAnn (EpAnn a' (AnnListItem []) emptyComments) spn
          x' = L ann' x
          xs' = fmap (L (mkRelSrcSpanAnn 1 spn (AnnListItem []))) xs
       in x' : xs'
    [] -> []


noAnnList :: AnnList
noAnnList = AnnList Nothing Nothing Nothing [] []

noAnnListItem :: AnnListItem
noAnnListItem = AnnListItem []

--
-- Modules
--

mkModule ::
  -- | Module name
  String ->
  -- | Pragmas
  [String] ->
  [ImportDecl GhcPs] ->
  [HsDecl GhcPs] ->
  HsModule GhcPs
mkModule name pragmas idecls decls =
  HsModule
    { hsmodExt =
        XModulePs
          { hsmodAnn = mkRelEpAnn (-1) s1 a1,
            hsmodLayout = VirtualBraces 1,
            hsmodDeprecMessage = Nothing,
            hsmodHaddockModHeader = Nothing
          },
      hsmodName = Just (L (mkRelSrcSpanAnn 0 s1 noAnnListItem) modName),
      hsmodExports = Nothing,
      hsmodImports = paragraphLines s1 idecls,
      hsmodDecls = paragraphLines s1 decls
    }
  where
    (s1, rs1) = defSrcSpan
    modName = ModuleName (fromString name)
    pragmaComments =
      let ls =
            fmap
              ( \p ->
                  let a = mkRelAnchor 1 s1
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

--
-- Imports
--

mkImport ::
  -- | Module name
  String ->
  ImportDecl GhcPs
mkImport name =
  ImportDecl
    { ideclExt = XImportDeclPass noAnn NoSourceText False,
      ideclName = L (mkRelSrcSpanAnn 0 s1 (AnnListItem [])) modName,
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
-- names
--

unqual :: OccName -> RdrName
unqual = Unqual

--
-- types
--

tycon :: String -> HsType GhcPs
tycon name =
  HsTyVar
    noAnn
    NotPromoted
    (L (mkRelSrcSpanAnn (-1) s1 (NameAnnTrailing [])) (unqual (mkTyVarOcc name)))
  where
    (s1, _) = defSrcSpan

-- TODO: deprecate this later
mkTVar :: String -> HsType GhcPs
mkTVar = tycon

tyapp :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
tyapp x y =
  HsAppTy noExtField lx ly
  where
    (s1, _) = defSrcSpan
    lx = L (mkRelSrcSpanAnn (-1) s1 (AnnListItem [])) x
    ly = L (mkRelSrcSpanAnn 0 s1 (AnnListItem [])) y

tylist :: HsType GhcPs -> HsType GhcPs
tylist x =
  HsListTy (mkRelEpAnn (-1) s1 ann) lx
  where
    (s1, _) = defSrcSpan
    ann =
      AnnParen
        { ap_adornment = AnnParensSquare,
          ap_open = EpaDelta (SameLine 0) [],
          ap_close = EpaDelta (SameLine 0) []
        }
    lx = L (mkRelSrcSpanAnn (-1) s1 (AnnListItem [])) x

--
-- Function
--

mkFun ::
  -- | function name
  String ->
  -- | function type
  HsType GhcPs ->
  -- | arg pattern
  [Pat GhcPs] ->
  -- | RHS
  HsExpr GhcPs ->
  -- | where
  Maybe (HsLocalBinds GhcPs) ->
  -- | decls
  [HsDecl GhcPs]
mkFun fname typ pats rhs mbinds =
  [ mkFunSig fname typ,
    mkBind1 fname pats rhs mbinds
  ]

mkFunSig ::
  -- | function name
  String ->
  HsType GhcPs ->
  HsDecl GhcPs
mkFunSig fname typ =
  SigD noExtField (TypeSig ann [lid] bndr)
  where
    (s1, rs1) = defSrcSpan
    ann =
      mkRelEpAnn (-1) s1 (AnnSig (AddEpAnn AnnDcolon (EpaDelta (SameLine 1) [])) [])

    id' = unqual (mkVarOcc fname)
    lid = L (mkRelSrcSpanAnn (-1) s1 (NameAnnTrailing [])) id'
    bndr = HsWC noExtField (L (mkRelSrcSpanAnn 0 s1 (AnnListItem [])) hsSigType)
    hsSigType =
      HsSig
        noExtField
        (HsOuterImplicit noExtField)
        (L (mkRelSrcSpanAnn (-1) s1 noAnnListItem) typ)

mkBind1 ::
  String ->
  [Pat GhcPs] ->
  HsExpr GhcPs ->
  Maybe (HsLocalBinds GhcPs) ->
  HsDecl GhcPs
mkBind1 fname pats rhs mbinds =
  ValD noExtField (FunBind noExtField lid payload)
  where
    (s1, rs1) = defSrcSpan
    id' = unqual (mkVarOcc fname)
    lid = L (mkRelSrcSpanAnn (-1) s1 (NameAnnTrailing [])) id'

    lpats = [] -- fmap (L ) pats
    lrhs = L (mkRelSrcSpanAnn (-1) s1 noAnnListItem) rhs
    glrhs =
      let ann =
            mkRelEpAnn
              (-1)
              s1
              (GrhsAnn Nothing (AddEpAnn AnnEqual (EpaDelta (SameLine 1) [])))
       in GRHS ann [] (lrhs)
    lglrhs = L (mkRelSrcSpanAnn (-1) s1 NoEpAnns) glrhs
    match =
      Match
        { m_ext =
            mkRelEpAnn
              (-1)
              s1
              [],
          m_ctxt = FunRhs lid Prefix NoSrcStrict,
          m_pats = lpats,
          m_grhss =
            GRHSs
              { grhssExt = emptyComments,
                grhssGRHSs = [lglrhs],
                grhssLocalBinds = EmptyLocalBinds noExtField
              }
        }
    lmatch = L (mkRelSrcSpanAnn (-1) s1 noAnnListItem) match
    payload = MG FromSource (L (mkRelSrcSpanAnn (-1) s1 noAnnList) [lmatch])
    -- [Match (Ident () n) pat (UnGuardedRhs () rhs) mbinds]

--              [ AddEpAnn AnnEqual (EpaDelta (SameLine 1) [])
--              ],


--
-- Expr
--

app :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
app x y =
  HsApp (mkRelEpAnn (-1) s1 NoEpAnns) lx ly
  where
    (s1, _) = defSrcSpan
    lx = L (mkRelSrcSpanAnn (-1) s1 noAnnListItem) x
    ly = L (mkRelSrcSpanAnn 0 s1 noAnnListItem) y

mkVar :: String -> HsExpr GhcPs
mkVar name =
  HsVar noExtField lid
  where
    (s1, _) = defSrcSpan
    id' = unqual (mkVarOcc name)
    lid = L (mkRelSrcSpanAnn (-1) s1 (NameAnnTrailing [])) id'

doE :: [StmtLR GhcPs GhcPs (LHsExpr GhcPs)] -> HsExpr GhcPs
doE stmts =
  HsDo
    (mkRelEpAnn (-1) s1 annDo)
    (DoExpr Nothing)
    llstmts
  where
    (s1, _) = defSrcSpan
    annDo =
      AnnList
        Nothing
        Nothing
        Nothing
        [AddEpAnn AnnDo (EpaDelta (SameLine 1) [])]
        []
    lstmts =
      paragraphLines' s1 (DifferentLine 1 2) stmts
    llstmts =
      let ann = mkRelSrcSpanAnn (-1) s1 noAnnList
       in L ann lstmts

listE :: [HsExpr GhcPs] -> HsExpr GhcPs
listE itms =
  case itms of
    -- NOTE: More correct way is to use GHC.Builtin.Names, but for codegen, this is enough.
    [] -> mkVar "[]"
    _ : _ ->
      let ann =
            AnnList
              Nothing
              Nothing
              Nothing
              [ AddEpAnn AnnOpenS (EpaDelta (SameLine 0) []),
                AddEpAnn AnnCloseS (EpaDelta (SameLine 0) [])
              ]
              []
          litms = fmap (L (mkRelSrcSpanAnn (-1) s1 noAnnListItem)) itms
       in ExplicitList (mkRelEpAnn (-1) s1 ann) litms
  where
    (s1, _) = defSrcSpan

mkBodyStmt :: HsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkBodyStmt expr =
  BodyStmt noExtField body noExtField noExtField
  where
    (s1, _) = defSrcSpan
    body = L (mkRelSrcSpanAnn (-1) s1 noAnnListItem) expr


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

infixl 2 `tyapp`

tyfun :: Type () -> Type () -> Type ()
tyfun = TyFun ()

infixr 2 `tyfun`

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

listE :: [Exp ()] -> Exp ()
listE = LHE.listE

strE :: String -> Exp ()
strE = LHE.strE

qualStmt :: Exp () -> Stmt ()
qualStmt = LHE.qualStmt

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
