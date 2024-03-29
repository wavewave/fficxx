{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Util.GHCExactPrint
  ( -- * DeclGroup
    DeclGroup (..),

    -- * utilities
    exactPrint,

    -- * module
    mkModule,
    mkModuleE,

    -- * import/export and FFI
    eabs,
    ethingall,
    ethingwith,
    evar,
    emodule,
    eWildCard,
    mkImport,
    mkImportSrc,
    mkForImpCcall,
    forD,

    -- * names
    unqual,

    -- * types
    mkTVar,
    tyForall,
    qualTy,
    tyapp,
    tycon,
    tyfun,
    tylist,
    tyParen,
    tyPtr,
    tyTupleBoxed,
    unit_tycon,
    mkTBind,

    -- * data/newtype declaration
    mkData,
    mkNewtype,
    conDecl,
    mkDeriving,

    -- * function
    mkFun,
    mkFun_,
    mkFunSig,
    mkBind,
    mkBind1,
    mkBind1_,

    -- * Typeclass
    cxEmpty,
    cxTuple,
    classA,
    mkClass,
    mkInstance,
    mkTypeFamInst,
    instD,

    -- * pattern
    mkPVar,
    mkPVarSig,
    pApp,
    pTuple,
    parP,
    wildcard,

    -- * expr
    app,
    caseE,
    con,
    doE,
    inapp,
    lamE,
    letE,
    listE,
    mkVar,
    mkVarWithComment,
    op,
    par,
    strE,
    tupleE,
    valBinds,
    toLocalBinds,

    -- * stmt
    mkBindStmt,
    mkBodyStmt,
    mkLetStmt,
    pbind,
    pbind_,

    -- * template haskell expr
    bracketExp,
    parenSplice,
    typeBracket,
    tySplice,

    -- * doc
    dummyComment,
    comment,
  )
where

import Data.Foldable (toList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.String (IsString (fromString))
import GHC.Data.Bag (emptyBag, listToBag)
import GHC.Hs
  ( AnnSig (..),
    AnnsModule (..),
    EpAnnHsCase (..),
    GhcPs,
    GrhsAnn (..),
    HsDocString (MultiLineDocString),
    HsDocStringChunk (..),
    HsDocStringDecorator (HsDocStringNext),
    WithHsDocIdentifiers (..),
    XImportDeclPass (..),
    XModulePs (..),
  )
import GHC.Parser.Annotation
  ( AddEpAnn (..),
    Anchor (..),
    AnchorOperation (..),
    AnnContext (..),
    AnnKeywordId (..),
    AnnList (..),
    AnnListItem (..),
    AnnParen (..),
    AnnSortKey (..),
    DeltaPos (..),
    EpAnn (..),
    EpAnnComments (EpaComments),
    EpaComment (..),
    EpaCommentTok (..),
    EpaLocation (..),
    IsUnicodeSyntax (NormalSyntax),
    NameAnn (..),
    NoEpAnns (..),
    ParenType (AnnParens, AnnParensSquare),
    SrcAnn,
    SrcSpanAnn' (SrcSpanAnn),
    SrcSpanAnnA,
    TokenLocation (..),
    TrailingAnn (..),
    emptyComments,
    noAnn,
    spanAsAnchor,
  )
import GHC.Types.Basic
  ( Origin (FromSource),
  )
import GHC.Types.Fixity
  ( LexicalFixity (Prefix),
  )
import GHC.Types.ForeignCall
  ( CCallConv (..),
    CCallTarget (StaticTarget),
    Safety (..),
  )
import GHC.Types.Name.Occurrence
  ( OccName,
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
    srcLocSpan,
  )
import qualified Language.Haskell.GHC.ExactPrint as Exact
import Language.Haskell.Syntax
  ( CImportSpec (CFunction),
    ClsInstDecl (..),
    ConDecl (..),
    DataDefnCons (..),
    DerivClauseTys (..),
    DocDecl (..),
    FamEqn (..),
    ForeignDecl (..),
    ForeignImport (CImport),
    GRHS (..),
    GRHSs (..),
    HsArg (..),
    HsArrow (..),
    HsBind,
    HsBindLR (..),
    HsConDetails (PrefixCon),
    HsContext,
    HsDataDefn (..),
    HsDecl (..),
    HsDeriving,
    HsDerivingClause (..),
    HsDoFlavour (..),
    HsExpr (..),
    HsForAllTelescope (..),
    HsLit (..),
    HsLocalBinds,
    HsLocalBindsLR (..),
    HsMatchContext (CaseAlt, FunRhs, LambdaExpr),
    HsModule (..),
    HsOuterTyVarBndrs (HsOuterImplicit),
    HsPatSigType (..),
    HsQuote (..),
    HsScaled (..),
    HsSigType (..),
    HsToken (..),
    HsTupArg (Present),
    HsTupleSort (..),
    HsTyVarBndr (UserTyVar),
    HsType (..),
    HsUniToken (..),
    HsUntypedSplice (..),
    HsValBinds,
    HsValBindsLR (..),
    HsWildCardBndrs (HsWC),
    IE (..),
    IEWildcard (..),
    IEWrappedName (..),
    ImportDecl (..),
    ImportDeclQualifiedStyle (..),
    InstDecl (..),
    IsBootInterface (..),
    LHsDecl,
    LHsExpr,
    LHsQTyVars (..),
    LIdP,
    LayoutInfo (..),
    Match (..),
    MatchGroup (..),
    ModuleName (..),
    Pat (..),
    PromotionFlag (..),
    Sig (TypeSig),
    StmtLR (..),
    TyClDecl (..),
    TyFamInstDecl (..),
    noExtField,
    noTypeArgs,
  )
import Language.Haskell.Syntax.Basic
  ( Boxity (..),
    SrcStrictness (NoSrcStrict),
  )

--
-- DeclGroup
--

data DeclGroup
  = DeclGroup [HsDecl GhcPs]
  | Comment String

--
-- utilities
--

-- | exact print
exactPrint :: (Exact.ExactPrint ast) => ast -> String
exactPrint = Exact.exactPrint . Exact.makeDeltaAst

mkDeltaPos :: Int -> DeltaPos
mkDeltaPos nLines
  | nLines < -1 = error "mkDeltaPos: cannot go backward further"
  | nLines == -1 = SameLine 0
  | nLines == 0 = SameLine 1
  | otherwise = DifferentLine nLines 0

mkEpaDelta :: Int -> EpaLocation
mkEpaDelta nLines = EpaDelta (mkDeltaPos nLines) []

tokLoc :: Int -> TokenLocation
tokLoc nLines = TokenLoc (mkEpaDelta nLines)

mkRelAnchor :: Int -> Anchor
mkRelAnchor nLines = mkRelAnchor' (mkDeltaPos nLines)

mkRelAnchor' :: DeltaPos -> Anchor
mkRelAnchor' delta =
  let a' = spanAsAnchor defSrcSpan
   in a' {anchor_op = MovedAnchor delta}

mkRelEpAnn :: Int -> ann -> EpAnn ann
mkRelEpAnn nLines = mkRelEpAnn' (mkDeltaPos nLines)

mkRelEpAnn' :: DeltaPos -> ann -> EpAnn ann
mkRelEpAnn' delta ann =
  EpAnn (mkRelAnchor' delta) ann emptyComments

mkRelSrcSpanAnn :: Int -> ann -> SrcAnn ann
mkRelSrcSpanAnn nLines ann =
  SrcSpanAnn (mkRelEpAnn nLines ann) defSrcSpan

mkRelSrcSpanAnn' :: DeltaPos -> ann -> SrcAnn ann
mkRelSrcSpanAnn' delta ann =
  SrcSpanAnn (mkRelEpAnn' delta ann) defSrcSpan

defSrcSpan :: SrcSpan
defSrcSpan = spn
  where
    sloc = mkSrcLoc "test" 1 1
    spn = srcLocSpan sloc

defRealSrcSpan :: RealSrcSpan
defRealSrcSpan = rspn
  where
    RealSrcSpan rspn _ = defSrcSpan

paragraphLines :: [a] -> [GenLocated SrcSpanAnnA a]
paragraphLines zs =
  case zs of
    x : xs ->
      let x' = L (mkRelSrcSpanAnn 2 (AnnListItem [])) x
          xs' = fmap (L (mkRelSrcSpanAnn 1 (AnnListItem []))) xs
       in x' : xs'
    [] -> []

-- | can place the group of items with arbitrary indentation.
paragraphLines' :: DeltaPos -> [a] -> [GenLocated SrcSpanAnnA a]
paragraphLines' delta zs =
  case zs of
    x : xs ->
      let a = spanAsAnchor defSrcSpan
          a' = a {anchor_op = MovedAnchor delta}
          ann' = SrcSpanAnn (EpAnn a' (AnnListItem []) emptyComments) defSrcSpan
          x' = L ann' x
          xs' = fmap (L (mkRelSrcSpanAnn 1 (AnnListItem []))) xs
       in x' : xs'
    [] -> []

noAnnList :: AnnList
noAnnList = AnnList Nothing Nothing Nothing [] []

noAnnListItem :: AnnListItem
noAnnListItem = AnnListItem []

mkL :: Int -> a -> GenLocated SrcSpanAnnA a
mkL nLines = L (mkRelSrcSpanAnn nLines noAnnListItem)

mkL' :: DeltaPos -> a -> GenLocated SrcSpanAnnA a
mkL' delta = L anno'
  where
    a = spanAsAnchor defSrcSpan
    a' = a {anchor_op = MovedAnchor delta}
    anno' = SrcSpanAnn (EpAnn a' (AnnListItem []) emptyComments) defSrcSpan

formatDeclGroup :: DeclGroup -> [LHsDecl GhcPs]
formatDeclGroup (DeclGroup decls) = paragraphLines decls
formatDeclGroup (Comment str) =
  [L (SrcSpanAnn epann defSrcSpan) (DocD noExtField dummyComment)]
  where
    epann = EpAnn (mkRelAnchor (-1)) noAnnListItem lcmts
    lcmts = EpaComments [L (mkRelAnchor 2) cmt]
    cmt =
      EpaComment
        (EpaLineComment str)
        defRealSrcSpan

--
-- Modules
--

mkModule ::
  -- | Module name
  String ->
  -- | Pragmas
  [String] ->
  -- | imports
  [ImportDecl GhcPs] ->
  -- | body of the module (separated in groups)
  [DeclGroup] ->
  -- | resultant HsModule
  HsModule GhcPs
mkModule name pragmas idecls decls = mkModuleE name pragmas Nothing idecls decls

mkModuleE ::
  -- | Module name
  String ->
  -- | Pragmas
  [String] ->
  -- | module exports
  Maybe [IE GhcPs] ->
  -- | imports
  [ImportDecl GhcPs] ->
  -- | body of the module (separated in groups)
  [DeclGroup] ->
  -- | resultant HsModule
  HsModule GhcPs
mkModuleE name pragmas mies idecls declss =
  HsModule
    { hsmodExt =
        XModulePs
          { hsmodAnn = mkRelEpAnn (-1) a1,
            hsmodLayout = VirtualBraces 1,
            hsmodDeprecMessage = Nothing,
            hsmodHaddockModHeader = Nothing
          },
      hsmodName = Just (L (mkRelSrcSpanAnn 0 noAnnListItem) modName),
      hsmodExports =
        fmap (L (mkRelSrcSpanAnn (-1) annExport) . tupleAnn) mies,
      hsmodImports = paragraphLines idecls,
      hsmodDecls = ldecls
    }
  where
    ldecls = concatMap formatDeclGroup declss
    modName = ModuleName (fromString name)
    pragmaComments =
      let ls =
            fmap
              ( \p ->
                  let a = mkRelAnchor 1
                      str = "{-# LANGUAGE " <> p <> " #-}"
                      c = EpaComment (EpaLineComment str) defRealSrcSpan
                   in L a c
              )
              pragmas
       in ls
    a1 =
      AnnsModule
        [ AddEpAnn AnnModule (EpaDelta (mkDeltaPos 2) pragmaComments),
          AddEpAnn AnnWhere (mkEpaDelta 0)
        ]
        (AnnList Nothing Nothing Nothing [] [])
    annExport =
      AnnList
        Nothing
        (Just (AddEpAnn AnnOpenP (mkEpaDelta 0)))
        (Just (AddEpAnn AnnCloseP (mkEpaDelta (-1))))
        []
        []

eabs :: String -> IE GhcPs
eabs name =
  IEThingAbs
    (mkRelEpAnn (-1) [])
    (mkL (-1) (IEName noExtField (mkLIdP (-1) name)))

ethingall :: String -> IE GhcPs
ethingall name =
  IEThingAll
    (mkRelEpAnn (-1) ann)
    (mkL (-1) (IEName noExtField (mkLIdP (-1) name)))
  where
    ann =
      [ AddEpAnn AnnOpenP (mkEpaDelta (-1)),
        AddEpAnn AnnCloseP (mkEpaDelta (-1)),
        AddEpAnn AnnDotdot (mkEpaDelta (-1))
      ]

ethingwith :: IEWildcard -> String -> [String] -> IE GhcPs
ethingwith wild name subs =
  IEThingWith
    (mkRelEpAnn (-1) ann)
    (mkWrappedName name)
    wild
    (fmap mkWrappedName subs)
  where
    ann =
      [ AddEpAnn AnnOpenP (mkEpaDelta (-1)),
        AddEpAnn AnnCloseP (mkEpaDelta (-1))
      ]
    mkWrappedName = mkL (-1) . IEName noExtField . mkLIdP (-1)

evar :: String -> IE GhcPs
evar name =
  IEVar noExtField (mkL (-1) (IEName noExtField (mkLIdP (-1) name)))

emodule :: String -> IE GhcPs
emodule name =
  IEModuleContents
    (mkRelEpAnn (-1) annos)
    (mkL 0 modName)
  where
    modName = ModuleName (fromString name)
    annos =
      [AddEpAnn AnnModule (mkEpaDelta (-1))]

eWildCard :: Int -> IEWildcard
eWildCard = IEWildcard

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
      ideclName = L (mkRelSrcSpanAnn 0 (AnnListItem [])) modName,
      ideclPkgQual = NoRawPkgQual,
      ideclSource = NotBoot,
      ideclSafe = False,
      ideclQualified = NotQualified,
      ideclAs = Nothing,
      ideclImportList = Nothing
    }
  where
    modName = ModuleName (fromString name)

mkImportSrc ::
  -- | Module name
  String ->
  ImportDecl GhcPs
mkImportSrc name =
  ImportDecl
    { ideclExt = XImportDeclPass noAnn NoSourceText False,
      ideclName = L (mkRelSrcSpanAnn 0 (AnnListItem [])) modName,
      ideclPkgQual = NoRawPkgQual,
      ideclSource = IsBoot,
      ideclSafe = False,
      ideclQualified = NotQualified,
      ideclAs = Nothing,
      ideclImportList = Nothing
    }
  where
    modName = ModuleName (fromString name)

-- NOTE: Unfortunately, the location annotation of GHC API for foreign import is not fully relative,
-- i.e. we cannot place correct spaces between "import", "ccall" and "safe", and the generated result
-- is not a valid Haskell code. So as a workaround we need to put a place holder in comment.
mkForImpCcall ::
  Safety ->
  String ->
  String ->
  HsType GhcPs ->
  ForeignDecl GhcPs
mkForImpCcall safety quote fname typ =
  ForeignImport (mkRelEpAnn (-1) annos) lid lsigty forImp
  where
    quote' = show quote
    for_imp_header =
      "foreign import ccall "
        <> case safety of
          PlayRisky -> "unsafe"
          PlaySafe -> "safe"
          PlayInterruptible -> "interruptible"
    annos =
      [ AddEpAnn
          AnnForeign
          ( EpaDelta
              (mkDeltaPos (-1))
              [ L
                  (mkRelAnchor 0)
                  ( EpaComment
                      ( EpaBlockComment
                          ( "{- REPLACE_THIS_LINE |"
                              <> for_imp_header
                              <> " \""
                              <> quote
                              <> "\"| -}"
                          )
                      )
                      defRealSrcSpan
                  )
              ]
          ),
        AddEpAnn AnnImport (mkEpaDelta 0),
        AddEpAnn AnnDcolon (mkEpaDelta 0)
      ]
    id' = unqual (mkVarOcc fname)
    lid =
      let a = spanAsAnchor defSrcSpan
          a' = a {anchor_op = MovedAnchor (DifferentLine 1 2)}
       in L (SrcSpanAnn (EpAnn a' (NameAnnTrailing []) emptyComments) defSrcSpan) id'
    outer = HsOuterImplicit noExtField
    sigty = HsSig noExtField outer (mkL (-1) typ)
    lsigty = mkL 0 sigty
    forImp =
      CImport
        (L defSrcSpan {- anchor_op = MovedAnchor (SameLine 1) -} (SourceText quote'))
        (L defSrcSpan {- anchor_op = MovedAnchor (SameLine 1) -} StdCallConv)
        (L defSrcSpan {- anchor_op = MovedAnchor (SameLine 1) -} safety)
        Nothing
        ( CFunction
            (StaticTarget (SourceText quote) (fromString quote) Nothing True)
        )

forD :: ForeignDecl GhcPs -> HsDecl GhcPs
forD = ForD noExtField

--
-- names
--

unqual :: OccName -> RdrName
unqual = Unqual

mkLIdP :: Int -> String -> LIdP GhcPs
mkLIdP nLines name = L (mkRelSrcSpanAnn nLines (NameAnnTrailing [])) id'
  where
    id' = unqual (mkVarOcc name)

--
-- types
--

tyForall ::
  NonEmpty (HsTyVarBndr () GhcPs) ->
  HsType GhcPs ->
  HsType GhcPs
tyForall tbnds typ =
  HsForAllTy
    { hst_xforall = noExtField,
      hst_tele = tele,
      hst_body = mkL (-1) typ
    }
  where
    ann = (AddEpAnn AnnForall (mkEpaDelta (-1)), AddEpAnn AnnDot (mkEpaDelta (-1)))
    tele = HsForAllVis (mkRelEpAnn (-1) ann) (fmap (mkL 0) $ toList tbnds)

qualTy ::
  HsContext GhcPs ->
  HsType GhcPs ->
  HsType GhcPs
qualTy ctxt typ =
  HsQualTy
    { hst_xqual = noExtField,
      hst_ctxt = L (mkRelSrcSpanAnn (-1) annCtxt) ctxt,
      hst_body = mkL 0 typ
    }
  where
    annCtxt
      | null ctxt = AnnContext Nothing [] []
      | otherwise =
          AnnContext
            { ac_darrow = Just (NormalSyntax, mkEpaDelta 0),
              ac_open = [mkEpaDelta 0],
              ac_close = [mkEpaDelta (-1)]
            }

tycon :: String -> HsType GhcPs
tycon name =
  HsTyVar
    noAnn
    NotPromoted
    (L (mkRelSrcSpanAnn (-1) (NameAnnTrailing [])) (unqual (mkTyVarOcc name)))

-- TODO: deprecate this later
mkTVar :: String -> HsType GhcPs
mkTVar = tycon

tyapp :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
tyapp x y =
  HsAppTy noExtField lx ly
  where
    lx = mkL (-1) x
    ly = mkL 0 y

infixl 2 `tyapp`

tyfun :: HsType GhcPs -> HsType GhcPs -> HsType GhcPs
tyfun x y =
  HsFunTy ann arrow lx ly
  where
    ann = mkRelEpAnn (-1) NoEpAnns
    arrow = HsUnrestrictedArrow (L (tokLoc 0) HsNormalTok)
    lx = mkL (-1) x
    ly = mkL 0 y

infixr 2 `tyfun`

tylist :: HsType GhcPs -> HsType GhcPs
tylist x =
  HsListTy (mkRelEpAnn (-1) ann) lx
  where
    ann =
      AnnParen
        { ap_adornment = AnnParensSquare,
          ap_open = mkEpaDelta (-1),
          ap_close = mkEpaDelta (-1)
        }
    lx = mkL (-1) x

tyParen :: HsType GhcPs -> HsType GhcPs
tyParen typ =
  HsParTy (mkRelEpAnn (-1) ann) (mkL (-1) typ)
  where
    ann = AnnParen AnnParens (mkEpaDelta (-1)) (mkEpaDelta (-1))

tyPtr :: HsType GhcPs
tyPtr = tycon "Ptr"

tyTupleBoxed :: [HsType GhcPs] -> HsType GhcPs
tyTupleBoxed typs =
  HsTupleTy (mkRelEpAnn (-1) ann) HsBoxedOrConstraintTuple (tupleAnn typs)
  where
    ann = AnnParen AnnParens (mkEpaDelta (-1)) (mkEpaDelta (-1))

unit_tycon :: HsType GhcPs
unit_tycon =
  HsTupleTy (mkRelEpAnn (-1) ann) HsBoxedOrConstraintTuple []
  where
    ann = AnnParen AnnParens (mkEpaDelta (-1)) (mkEpaDelta (-1))

mkTBind :: String -> HsTyVarBndr () GhcPs
mkTBind name = UserTyVar (mkRelEpAnn (-1) []) () (mkLIdP (-1) name)

--
-- data/newtype declaration
--

mkData ::
  -- | data type name
  String ->
  [HsTyVarBndr () GhcPs] ->
  [ConDecl GhcPs] ->
  HsDeriving GhcPs ->
  TyClDecl GhcPs
mkData name tbinds cdecls deriv =
  DataDecl (mkRelEpAnn (-1) annos) (mkLIdP 0 name) qty Prefix dfn
  where
    annData = AddEpAnn AnnData (mkEpaDelta (-1))
    annEqual = AddEpAnn AnnEqual (mkEpaDelta 0)
    annos
      | null cdecls = [annData]
      | otherwise = [annData, annEqual]
    qty = HsQTvs noExtField $ fmap (mkL 0) tbinds
    dfn =
      HsDataDefn
        { dd_ext = noExtField,
          dd_ctxt = Nothing,
          dd_cType = Nothing,
          dd_kindSig = Nothing,
          dd_cons =
            let loc = EpaDelta (DifferentLine 1 2) []
             in DataTypeCons False (listSep' loc AddVbarAnn cdecls),
          dd_derivs = deriv
        }

mkNewtype ::
  -- | newtype name
  String ->
  [HsTyVarBndr () GhcPs] ->
  ConDecl GhcPs ->
  HsDeriving GhcPs ->
  TyClDecl GhcPs
mkNewtype name tbinds cdecl deriv =
  DataDecl (mkRelEpAnn (-1) annos) (mkLIdP 0 name) qty Prefix dfn
  where
    annos =
      [ AddEpAnn AnnNewtype (mkEpaDelta (-1)),
        AddEpAnn AnnEqual (mkEpaDelta 0)
      ]
    qty = HsQTvs noExtField $ fmap (mkL 0) tbinds
    dfn =
      HsDataDefn
        { dd_ext = noExtField,
          dd_ctxt = Nothing,
          dd_cType = Nothing,
          dd_kindSig = Nothing,
          dd_cons = NewTypeCon (mkL 0 cdecl),
          dd_derivs = deriv
        }

conDecl :: String -> [HsType GhcPs] -> ConDecl GhcPs
conDecl name typs =
  ConDeclH98
    { con_ext = mkRelEpAnn (-1) [],
      con_name = mkLIdP (-1) name,
      con_forall = False,
      con_ex_tvs = [],
      con_mb_cxt = Nothing,
      con_args = details,
      con_doc = Nothing
    }
  where
    details = PrefixCon noTypeArgs args
    args =
      fmap (HsScaled (HsUnrestrictedArrow (L NoTokenLoc HsNormalTok))) ltyps
    ltyps = fmap (mkL 0 . tyParen) typs

mkDeriving :: [HsType GhcPs] -> HsDeriving GhcPs
mkDeriving typs = [L (mkRelSrcSpanAnn 0 NoEpAnns) clause]
  where
    clause =
      HsDerivingClause
        { deriv_clause_ext =
            mkRelEpAnn
              (-1)
              [ AddEpAnn AnnDeriving (mkEpaDelta (-1))
              ],
          deriv_clause_strategy = Nothing,
          deriv_clause_tys =
            L
              ( mkRelSrcSpanAnn
                  (-1)
                  ( AnnContext
                      { ac_darrow = Nothing,
                        ac_open = [mkEpaDelta 0],
                        ac_close = [mkEpaDelta (-1)]
                      }
                  )
              )
              typs'
        }
    typs' = DctMulti noExtField (tupleAnn $ fmap mkSigTy typs)
    mkSigTy t =
      HsSig
        { sig_ext = noExtField,
          sig_bndrs = HsOuterImplicit noExtField,
          sig_body = mkL (-1) t
        }

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
  HsLocalBinds GhcPs ->
  -- | decls
  [HsDecl GhcPs]
mkFun fname typ pats rhs bnds =
  [ SigD noExtField (mkFunSig fname typ),
    ValD noExtField (mkBind1 fname pats rhs bnds)
  ]

mkFun_ ::
  -- | function name
  String ->
  -- | function type
  HsType GhcPs ->
  -- | arg pattern
  [Pat GhcPs] ->
  -- | RHS
  HsExpr GhcPs ->
  -- | decls
  [HsDecl GhcPs]
mkFun_ fname typ pats rhs = mkFun fname typ pats rhs (EmptyLocalBinds noExtField)

mkFunSig ::
  -- | function name
  String ->
  HsType GhcPs ->
  Sig GhcPs
mkFunSig fname typ =
  TypeSig ann [lid] bndr
  where
    ann =
      mkRelEpAnn (-1) (AnnSig (AddEpAnn AnnDcolon (mkEpaDelta 0)) [])

    id' = unqual (mkVarOcc fname)
    lid = L (mkRelSrcSpanAnn (-1) (NameAnnTrailing [])) id'
    bndr = HsWC noExtField (L (mkRelSrcSpanAnn 0 (AnnListItem [])) hsSigType)
    hsSigType =
      HsSig
        noExtField
        (HsOuterImplicit noExtField)
        (mkL (-1) typ)

mkBind ::
  DeltaPos ->
  String ->
  [([Pat GhcPs], HsExpr GhcPs, HsLocalBinds GhcPs)] ->
  HsBind GhcPs
mkBind delta fname matches =
  FunBind noExtField lid payload
  where
    id' = unqual (mkVarOcc fname)
    lid = L (mkRelSrcSpanAnn (-1) (NameAnnTrailing [])) id'
    matches' =
      fmap
        ( \(pats, rhs, bnds) ->
            mkMatch (FunRhs lid Prefix NoSrcStrict) pats rhs bnds
        )
        matches
    lmatches' = fmap (L (mkRelSrcSpanAnn' delta noAnnListItem)) matches' -- paragraphLines' (SameLine 0) matches'
    payload = MG FromSource (L (mkRelSrcSpanAnn (-1) noAnnList) lmatches')

mkBind1 ::
  String ->
  [Pat GhcPs] ->
  HsExpr GhcPs ->
  HsLocalBinds GhcPs ->
  HsBind GhcPs
mkBind1 fname pats rhs bnds =
  FunBind noExtField lid payload
  where
    id' = unqual (mkVarOcc fname)
    lid = L (mkRelSrcSpanAnn (-1) (NameAnnTrailing [])) id'
    match = mkMatch (FunRhs lid Prefix NoSrcStrict) pats rhs bnds
    lmatch = mkL (-1) match
    payload = MG FromSource (L (mkRelSrcSpanAnn (-1) noAnnList) [lmatch])

mkBind1_ ::
  String ->
  [Pat GhcPs] ->
  HsExpr GhcPs ->
  HsBind GhcPs
mkBind1_ fname pats rhs = mkBind1 fname pats rhs (EmptyLocalBinds noExtField)

listSep' ::
  EpaLocation ->
  (EpaLocation -> TrailingAnn) ->
  [a] ->
  [GenLocated SrcSpanAnnA a]
listSep' _ _ [] = []
listSep' _ _ (x : []) = [mkL (-1) x]
listSep' loc sep xs =
  let xs' = init xs
      lastX = last xs
      xs'' =
        fmap
          (L (mkRelSrcSpanAnn 0 (AnnListItem [sep loc])))
          xs'
   in (xs'' ++ [mkL 0 lastX])

listSep :: (EpaLocation -> TrailingAnn) -> [a] -> [GenLocated SrcSpanAnnA a]
listSep = listSep' (mkEpaDelta (-1))

tupleAnn :: [a] -> [GenLocated SrcSpanAnnA a]
tupleAnn = listSep AddCommaAnn

--
-- Typeclass
--

cxEmpty :: HsContext GhcPs
cxEmpty = []

cxTuple :: [HsType GhcPs] -> HsContext GhcPs
cxTuple = tupleAnn

classA :: String -> [HsType GhcPs] -> HsType GhcPs
classA name typs = foldl' tyapp (tycon name) typs'
  where
    typs' = fmap tyParen typs

mkClass ::
  HsContext GhcPs ->
  String ->
  [HsTyVarBndr () GhcPs] ->
  [Sig GhcPs] ->
  TyClDecl GhcPs
mkClass ctxt name tbnds sigs =
  ClassDecl
    { tcdCExt = (mkRelEpAnn (-1) annos, NoAnnSortKey),
      tcdLayout = VirtualBraces 2,
      tcdCtxt = Just (L (mkRelSrcSpanAnn 0 annCtxt) ctxt),
      tcdLName = mkLIdP 0 name,
      tcdTyVars = HsQTvs noExtField $ fmap (mkL 0) tbnds,
      tcdFixity = Prefix,
      tcdFDs = [],
      tcdSigs = fmap (mkL' (DifferentLine 1 2)) sigs,
      tcdMeths = emptyBag,
      tcdATs = [],
      tcdATDefs = [],
      tcdDocs = []
    }
  where
    annos =
      [ AddEpAnn AnnClass (mkEpaDelta (-1)),
        AddEpAnn AnnWhere (mkEpaDelta 0)
      ]
    annCtxt
      | null ctxt = AnnContext Nothing [] []
      | otherwise =
          AnnContext
            { ac_darrow = Just (NormalSyntax, mkEpaDelta 0),
              ac_open = [mkEpaDelta (-1)],
              ac_close = [mkEpaDelta (-1)]
            }

mkInstance ::
  -- | Context
  HsContext GhcPs ->
  -- | Typeclass name
  String ->
  -- | instance types
  [HsType GhcPs] ->
  -- | instance type family declarations
  [TyFamInstDecl GhcPs] ->
  -- | instance function definitions
  [HsBind GhcPs] ->
  -- | resultant declaration
  ClsInstDecl GhcPs
mkInstance ctxt name typs tyfams bnds =
  ClsInstDecl
    { cid_ext = ann,
      cid_poly_ty =
        mkL (-1) (HsSig noExtField (HsOuterImplicit noExtField) (mkL (-1) typcls)),
      cid_binds = bnds',
      cid_sigs = [],
      cid_tyfam_insts = fmap (mkL' (DifferentLine 1 2)) tyfams,
      cid_datafam_insts = [],
      cid_overlap_mode = Nothing
    }
  where
    bnds' = listToBag $ fmap (L ann') bnds
      where
        a = spanAsAnchor defSrcSpan
        a' = a {anchor_op = MovedAnchor (DifferentLine 1 2)}
        ann' = SrcSpanAnn (EpAnn a' noAnnListItem emptyComments) defSrcSpan
    ann =
      ( mkRelEpAnn
          1
          [ AddEpAnn AnnInstance (mkEpaDelta (-1)),
            AddEpAnn AnnWhere (mkEpaDelta 0)
          ],
        NoAnnSortKey
      )
    typcls =
      HsQualTy
        { hst_xqual = noExtField,
          hst_ctxt = L (mkRelSrcSpanAnn 0 annCtxt) ctxt,
          hst_body = mkL 0 insttyp
        }
    annCtxt
      | null ctxt = AnnContext Nothing [] []
      | otherwise =
          AnnContext
            { ac_darrow = Just (NormalSyntax, mkEpaDelta 0),
              ac_open = [mkEpaDelta (-1)],
              ac_close = [mkEpaDelta (-1)]
            }
    insttyp = foldl' f (tycon name) typs
      where
        f acc x = tyapp acc (tyParen x)

mkTypeFamInst :: String -> [HsType GhcPs] -> HsType GhcPs -> TyFamInstDecl GhcPs
mkTypeFamInst name args typ =
  TyFamInstDecl (mkRelEpAnn (-1) annos) eqn
  where
    annos =
      [ AddEpAnn AnnType (mkEpaDelta (-1))
      ]
    eqn =
      FamEqn
        { feqn_ext =
            mkRelEpAnn
              0
              [ AddEpAnn AnnEqual (mkEpaDelta 0)
              ],
          feqn_tycon = mkLIdP (-1) name,
          feqn_bndrs = HsOuterImplicit noExtField,
          feqn_pats = fmap (\t -> HsValArg (mkL 0 t)) args,
          feqn_fixity = Prefix,
          feqn_rhs = mkL 0 typ
        }

instD :: ClsInstDecl GhcPs -> HsDecl GhcPs
instD = InstD noExtField . ClsInstD noExtField

--
-- Pattern
--

mkPVar :: String -> Pat GhcPs
mkPVar name = VarPat noExtField (mkLIdP (-1) name)

mkPVarSig :: String -> HsType GhcPs -> Pat GhcPs
mkPVarSig name typ =
  SigPat
    (mkRelEpAnn (-1) annos)
    (mkL (-1) (mkPVar name))
    psig
  where
    annos =
      [ AddEpAnn AnnDcolon (mkEpaDelta 0)
      ]
    psig = HsPS (mkRelEpAnn (-1) NoEpAnns) (mkL 0 typ)

pApp :: String -> [Pat GhcPs] -> Pat GhcPs
pApp name pats =
  ConPat
    { pat_con_ext = mkRelEpAnn (-1) [],
      pat_con = mkLIdP (-1) name,
      pat_args = PrefixCon [] lpats
    }
  where
    lpats = fmap (mkL 0) pats

pTuple :: [Pat GhcPs] -> Pat GhcPs
pTuple ps =
  TuplePat (mkRelEpAnn (-1) annos) (tupleAnn ps) Boxed
  where
    annos =
      [ AddEpAnn AnnOpenP (mkEpaDelta (-1)),
        AddEpAnn AnnCloseP (mkEpaDelta (-1))
      ]

parP :: Pat GhcPs -> Pat GhcPs
parP p =
  ParPat
    (mkRelEpAnn (-1) NoEpAnns)
    (L (tokLoc (-1)) HsTok)
    (mkL (-1) p)
    (L (tokLoc (-1)) HsTok)

wildcard :: Pat GhcPs
wildcard = WildPat noExtField

--
-- Expr
--

app :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
app x y =
  HsApp (mkRelEpAnn (-1) NoEpAnns) lx ly
  where
    lx = mkL (-1) x
    ly = mkL 0 y

caseE :: HsExpr GhcPs -> [(Pat GhcPs, HsExpr GhcPs)] -> HsExpr GhcPs
caseE expr matches =
  HsCase (mkRelEpAnn (-1) ann) (mkL 0 expr) grp
  where
    ann =
      EpAnnHsCase
        { hsCaseAnnCase = mkEpaDelta (-1),
          hsCaseAnnOf = mkEpaDelta 0,
          hsCaseAnnsRest = []
        }
    grp = MG FromSource (L (mkRelSrcSpanAnn (-1) ann') matches')
    ann' =
      AnnList
        Nothing
        (Just (AddEpAnn AnnOpenC (mkEpaDelta (-1))))
        (Just (AddEpAnn AnnCloseC (mkEpaDelta (-1))))
        []
        []
    matches' =
      listSep AddSemiAnn $
        fmap (\(p, e) -> mkMatch CaseAlt [p] e (EmptyLocalBinds noExtField)) matches

-- NOTE: in ghc API, no difference between constructor and variable
con :: String -> HsExpr GhcPs
con = mkVar

doE :: [StmtLR GhcPs GhcPs (LHsExpr GhcPs)] -> HsExpr GhcPs
doE stmts =
  HsDo
    (mkRelEpAnn (-1) annDo)
    (DoExpr Nothing)
    llstmts
  where
    annDo =
      AnnList
        Nothing
        Nothing
        Nothing
        [AddEpAnn AnnDo (EpaDelta (SameLine 1) [])]
        []
    lstmts =
      paragraphLines' (DifferentLine 1 2) stmts
    llstmts =
      let ann = mkRelSrcSpanAnn (-1) noAnnList
       in L ann lstmts

inapp ::
  -- | left arg
  HsExpr GhcPs ->
  -- | operator
  HsExpr GhcPs ->
  -- | right arg
  HsExpr GhcPs ->
  HsExpr GhcPs
inapp x o y =
  OpApp ann lx lo ly
  where
    ann = mkRelEpAnn (-1) []
    lx = mkL (-1) x
    lo = mkL (-1) o
    ly = mkL (-1) y

mkMatch ::
  HsMatchContext GhcPs ->
  [Pat GhcPs] ->
  HsExpr GhcPs ->
  HsLocalBinds GhcPs ->
  Match GhcPs (LHsExpr GhcPs)
mkMatch mctxt pats rhs bnds =
  Match
    { m_ext = mkRelEpAnn (-1) annos,
      m_ctxt = mctxt,
      m_pats = lpats,
      m_grhss =
        GRHSs
          { grhssExt = emptyComments,
            grhssGRHSs = [lglrhs],
            grhssLocalBinds = bnds
          }
    }
  where
    annos =
      case mctxt of
        LambdaExpr -> [AddEpAnn AnnLam (mkEpaDelta (-1))]
        _ -> []
    lpats = fmap (mkL 0) pats
    lrhs = mkL 0 rhs
    glrhs =
      let ann = case mctxt of
            LambdaExpr -> AnnRarrow
            CaseAlt -> AnnRarrow
            _ -> AnnEqual
          ann' =
            mkRelEpAnn
              (-1)
              (GrhsAnn Nothing (AddEpAnn ann (mkEpaDelta 0)))
       in GRHS ann' [] (lrhs)
    lglrhs = L (mkRelSrcSpanAnn (-1) NoEpAnns) glrhs

lamE :: [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
lamE pats expr =
  HsLam noExtField grp
  where
    grp = MG FromSource (L (mkRelSrcSpanAnn (-1) annos) [mkL (-1) match])
    annos =
      AnnList
        Nothing
        (Just (AddEpAnn AnnOpenP (mkEpaDelta (-1))))
        (Just (AddEpAnn AnnCloseP (mkEpaDelta (-1))))
        []
        []
    match = mkMatch LambdaExpr pats expr (EmptyLocalBinds noExtField)

letE :: HsLocalBinds GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
letE bnds expr =
  HsLet (mkRelEpAnn' (DifferentLine 1 2) NoEpAnns) tokLet bnds tokIn (mkL 0 expr)
  where
    tokLet = L (tokLoc (-1)) HsTok
    tokIn = L (tokLoc 1) HsTok

listE :: [HsExpr GhcPs] -> HsExpr GhcPs
listE itms =
  case itms of
    -- NOTE: More correct way is to use GHC.Builtin.Names, but for codegen, this is enough.
    [] -> mkVar "[]"
    _ : _ ->
      let ann =
            AnnList
              Nothing
              (Just (AddEpAnn AnnOpenS (mkEpaDelta (-1))))
              (Just (AddEpAnn AnnCloseS (mkEpaDelta (-1))))
              []
              []
          litms = tupleAnn itms
       in ExplicitList (mkRelEpAnn (-1) ann) litms
  where

mkVar :: String -> HsExpr GhcPs
mkVar name =
  HsVar noExtField (mkLIdP (-1) name)

mkVarWithComment :: String -> String -> HsExpr GhcPs
mkVarWithComment name str =
  HsVar noExtField (L ann id')
  where
    id' = unqual (mkVarOcc name)
    cmt =
      EpaComment
        (EpaLineComment str)
        defRealSrcSpan
    lcmts = EpaComments [L (mkRelAnchor 0) cmt]
    ann =
      SrcSpanAnn (EpAnn (mkRelAnchor (-1)) (NameAnnTrailing []) lcmts) defSrcSpan

op :: String -> HsExpr GhcPs
op = mkVar

par :: HsExpr GhcPs -> HsExpr GhcPs
par expr =
  HsPar ann tokOpen (mkL (-1) expr) tokClose
  where
    ann = mkRelEpAnn (-1) NoEpAnns
    tokOpen = L (tokLoc (-1)) HsTok
    tokClose = L (tokLoc (-1)) HsTok

infixl 2 `par`

strE :: String -> HsExpr GhcPs
strE str = HsLit ann1 (HsString ann2 (fromString str))
  where
    str' = show str
    ann1 = mkRelEpAnn (-1) NoEpAnns
    ann2 = SourceText str'

tupleE :: [HsExpr GhcPs] -> HsExpr GhcPs
tupleE exprs =
  ExplicitTuple (mkRelEpAnn (-1) annos) args Boxed
  where
    annos =
      [ AddEpAnn AnnOpenP (mkEpaDelta (-1)),
        AddEpAnn AnnCloseP (mkEpaDelta (-1))
      ]
    mkArg = Present EpAnnNotUsed
    args = fmap mkArg $ tupleAnn exprs

valBinds :: [HsBind GhcPs] -> HsValBinds GhcPs
valBinds bnds =
  ValBinds NoAnnSortKey (listToBag lbnds) []
  where
    lbnds = paragraphLines' (SameLine 2) bnds

toLocalBinds :: Bool -> HsValBinds GhcPs -> HsLocalBinds GhcPs
toLocalBinds withWhere =
  HsValBinds (mkRelEpAnn' (DifferentLine 1 2) ann)
  where
    ann
      | withWhere =
          AnnList
            Nothing
            Nothing
            Nothing
            [AddEpAnn AnnWhere (mkEpaDelta (-1))]
            []
      | otherwise = noAnnList

--
-- Statements
--

mkBindStmt :: Pat GhcPs -> HsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkBindStmt pat expr =
  BindStmt (mkRelEpAnn (-1) annos) (mkL (-1) pat) (mkL 0 expr)
  where
    annos =
      [AddEpAnn AnnLarrow (mkEpaDelta 0)]

mkBodyStmt :: HsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkBodyStmt expr =
  BodyStmt noExtField body noExtField noExtField
  where
    body = mkL (-1) expr

mkLetStmt :: [HsBind GhcPs] -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkLetStmt bnds =
  LetStmt (mkRelEpAnn (-1) annos) (toLocalBinds False $ valBinds bnds)
  where
    annos = [AddEpAnn AnnLet (mkEpaDelta (-1))]

pbind :: Pat GhcPs -> HsExpr GhcPs -> HsLocalBinds GhcPs -> HsBind GhcPs
pbind pat expr bnds =
  PatBind (mkRelEpAnn (-1) []) (mkL (-1) pat) grhss
  where
    grhss = GRHSs emptyComments [lgrhs] bnds
    lgrhs = L (mkRelSrcSpanAnn (-1) NoEpAnns) grhs
    grhs = GRHS (mkRelEpAnn (-1) ann) [] (mkL 0 expr)
    ann = GrhsAnn Nothing (AddEpAnn AnnEqual (mkEpaDelta 0))

pbind_ :: Pat GhcPs -> HsExpr GhcPs -> HsBind GhcPs
pbind_ p e = pbind p e (EmptyLocalBinds noExtField)

--
-- template haskell expr
--

bracketExp :: HsQuote GhcPs -> HsExpr GhcPs
bracketExp quote =
  HsUntypedBracket (mkRelEpAnn (-1) annos) quote
  where
    annos =
      [ AddEpAnn AnnOpen (mkEpaDelta (-1)),
        AddEpAnn AnnCloseQ (mkEpaDelta (-1))
      ]

parenSplice :: HsExpr GhcPs -> HsUntypedSplice GhcPs
parenSplice expr =
  HsUntypedSpliceExpr (mkRelEpAnn (-1) annos) (mkL (-1) expr)
  where
    annos =
      [ AddEpAnn AnnDollar (mkEpaDelta (-1))
      ]

typeBracket :: HsType GhcPs -> HsQuote GhcPs
typeBracket typ =
  TypBr noExtField (mkL (-1) typ)

tySplice :: HsUntypedSplice GhcPs -> HsType GhcPs
tySplice sp = HsSpliceTy noExtField sp

--
-- doc
--

-- DocCommentNext content with dummy contents
dummyComment :: DocDecl GhcPs
dummyComment =
  DocCommentNext (L defSrcSpan (WithHsDocIdentifiers str []))
  where
    str =
      MultiLineDocString
        HsDocStringNext
        (NE.singleton (L defSrcSpan (HsDocStringChunk "")))

comment :: String -> DeclGroup
comment = Comment
