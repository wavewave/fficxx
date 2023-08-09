{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.Util.GHCExactPrint
  ( -- * module
    mkModule,

    -- * import and FFI
    mkImport,
    mkForImpCcall,

    -- * names
    unqual,

    -- * types
    mkTVar,
    tyapp,
    tycon,
    tyfun,
    tylist,
    tyParen,
    tyPtr,
    unit_tycon,

    -- * data/newtype declaration
    mkData,
    mkNewtype,
    conDecl,

    -- * function
    mkFun,
    mkFunSig,
    mkBind1,

    -- * Typeclass
    cxEmpty,
    cxTuple,
    classA,
    mkInstance,
    instD,

    -- * pattern
    mkPVar,
    pApp,

    -- * expr
    app,
    con,
    doE,
    inapp,
    listE,
    mkVar,
    op,
    par,
    strE,

    -- * stmt
    mkBodyStmt,
    {- app',
    conDecl,
    qualConDecl,
    recDecl,
    lit,
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
    mkModuleE,
    mkImportExp,
    mkImportSrc,
    lang,
    dot,
    tyForall,
    tyParen,
    tyForeignPtr,
    classA,
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
import GHC.Data.Bag (listToBag)
import GHC.Hs
  ( AnnSig (..),
    AnnsModule (..),
    GhcPs,
    GrhsAnn (..),
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
    noSrcSpanA,
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
    CImportSpec (CFunction),
    ClsInstDecl (..),
    ConDecl (..),
    DataDefnCons (..),
    ExprLStmt,
    ForeignDecl (..),
    ForeignImport (CImport),
    GRHS (..),
    GRHSs (..),
    HsArrow (..),
    HsBind (..),
    HsBindLR (..),
    HsConDetails (PrefixCon),
    HsContext (..),
    HsDataDefn (..),
    HsDecl (..),
    HsDeriving (..),
    HsDoFlavour (..),
    HsExpr (..),
    HsLit (..),
    HsLocalBinds,
    HsLocalBindsLR (..),
    HsMatchContext (FunRhs),
    HsModule (..),
    HsOuterTyVarBndrs (HsOuterImplicit),
    HsScaled (..),
    HsSigType (HsSig),
    HsToken (..),
    HsTupleSort (..),
    HsType (..),
    HsUniToken (..),
    HsWildCardBndrs (HsWC),
    ImportDecl (..),
    ImportDeclQualifiedStyle (..),
    InstDecl (..),
    IsBootInterface (..),
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
    noExtField,
    noTypeArgs,
  )
import Language.Haskell.Syntax.Basic
  ( SrcStrictness (NoSrcStrict),
  )

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
mkRelAnchor nLines =
  let a' = spanAsAnchor defSrcSpan
   in a' {anchor_op = MovedAnchor (mkDeltaPos nLines)}

mkRelEpAnn :: Int -> ann -> EpAnn ann
mkRelEpAnn nLines ann = EpAnn (mkRelAnchor nLines) ann emptyComments

mkRelSrcSpanAnn :: Int -> ann -> SrcAnn ann
mkRelSrcSpanAnn nLines ann =
  SrcSpanAnn (mkRelEpAnn nLines ann) defSrcSpan

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
          { hsmodAnn = mkRelEpAnn (-1) a1,
            hsmodLayout = VirtualBraces 1,
            hsmodDeprecMessage = Nothing,
            hsmodHaddockModHeader = Nothing
          },
      hsmodName = Just (L (mkRelSrcSpanAnn 0 noAnnListItem) modName),
      hsmodExports = Nothing,
      hsmodImports = paragraphLines idecls,
      hsmodDecls = paragraphLines decls
    }
  where
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

-- NOTE: Unfortunately, the location annotation of GHC API for foreign import is not fully relative,
-- i.e. we cannot place correct spaces between "import", "ccall" and "safe", and the generated result
-- is not a valid Haskell code. So as a workaround we need to put a place holder in comment.
mkForImpCcall :: String -> String -> HsType GhcPs -> ForeignDecl GhcPs
mkForImpCcall quote fname typ =
  ForeignImport (mkRelEpAnn (-1) anns) lid lsigty forImp
  where
    quote' = show quote
    anns =
      [ AddEpAnn
          AnnForeign
          ( EpaDelta
              (mkDeltaPos (-1))
              [ L
                  (mkRelAnchor 0)
                  ( EpaComment
                      ( EpaBlockComment
                          ( "{- REPLACE_THIS_LINE |foreign import ccall interruptible \""
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
        (L defSrcSpan {- anchor_op = MovedAnchor (SameLine 1) -} PlayInterruptible)
        Nothing
        ( CFunction
            (StaticTarget (SourceText quote) (fromString quote) Nothing True)
        )

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
    arrow = HsUnrestrictedArrow (L (tokLoc (-1)) HsNormalTok)
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

unit_tycon :: HsType GhcPs
unit_tycon =
  HsTupleTy (mkRelEpAnn (-1) ann) HsBoxedOrConstraintTuple []
  where
    ann = AnnParen AnnParens (mkEpaDelta (-1)) (mkEpaDelta (-1))

--
-- data/newtype declaration
--

mkData ::
  -- | data type name
  String ->
  -- [TyVarBind ()] ->
  [ConDecl GhcPs] ->
  HsDeriving GhcPs ->
  TyClDecl GhcPs
mkData name {- tbinds -} cdecls deriv =
  DataDecl (mkRelEpAnn (-1) anns) (mkLIdP 0 name) qty Prefix dfn
  where
    anns =
      [ AddEpAnn AnnData (mkEpaDelta (-1))
      ]
    qty = HsQTvs noExtField []
    dfn =
      HsDataDefn
        { dd_ext = noExtField,
          dd_ctxt = Nothing,
          dd_cType = Nothing,
          dd_kindSig = Nothing,
          dd_cons = DataTypeCons False (fmap (mkL (-1)) cdecls),
          dd_derivs = deriv
        }

{-
  DataDecl () (DataType ()) Nothing declhead qdecls (maybeToList mderiv)
  where
    declhead = mkDeclHead n tbinds
-}

mkNewtype ::
  -- | newtype name
  String ->
  -- [TyVarBind ()] ->
  ConDecl GhcPs ->
  HsDeriving GhcPs ->
  TyClDecl GhcPs
mkNewtype name {- tbinds -} cdecl deriv =
  DataDecl (mkRelEpAnn (-1) anns) (mkLIdP 0 name) qty Prefix dfn
  where
    anns =
      [ AddEpAnn AnnNewtype (mkEpaDelta (-1))
      ]
    qty = HsQTvs noExtField []
    dfn =
      HsDataDefn
        { dd_ext = noExtField,
          dd_ctxt = Nothing,
          dd_cType = Nothing,
          dd_kindSig = Nothing,
          dd_cons = NewTypeCon (mkL (-1) cdecl),
          dd_derivs = deriv
        }

{- DataDecl () (NewType ()) Nothing declhead qdecls (maybeToList mderiv)
  where
    declhead = mkDeclHead n tbinds
-}

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
      fmap (HsScaled (HsUnrestrictedArrow (L (tokLoc (-1)) HsNormalTok))) ltyps
    ltyps = fmap (mkL (-1)) typs

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
    ValD noExtField (mkBind1 fname pats rhs mbinds)
  ]

mkFunSig ::
  -- | function name
  String ->
  HsType GhcPs ->
  HsDecl GhcPs
mkFunSig fname typ =
  SigD noExtField (TypeSig ann [lid] bndr)
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

mkBind1 ::
  String ->
  [Pat GhcPs] ->
  HsExpr GhcPs ->
  Maybe (HsLocalBinds GhcPs) ->
  HsBind GhcPs
mkBind1 fname pats rhs mbinds =
  FunBind noExtField lid payload
  where
    id' = unqual (mkVarOcc fname)
    lid = L (mkRelSrcSpanAnn (-1) (NameAnnTrailing [])) id'

    lpats = fmap (mkL 0) pats
    lrhs = mkL 0 rhs
    glrhs =
      let ann =
            mkRelEpAnn
              (-1)
              (GrhsAnn Nothing (AddEpAnn AnnEqual (mkEpaDelta 0)))
       in GRHS ann [] (lrhs)
    lglrhs = L (mkRelSrcSpanAnn (-1) NoEpAnns) glrhs
    match =
      Match
        { m_ext = mkRelEpAnn (-1) [],
          m_ctxt = FunRhs lid Prefix NoSrcStrict,
          m_pats = lpats,
          m_grhss =
            GRHSs
              { grhssExt = emptyComments,
                grhssGRHSs = [lglrhs],
                grhssLocalBinds = EmptyLocalBinds noExtField
              }
        }
    lmatch = mkL (-1) match
    payload = MG FromSource (L (mkRelSrcSpanAnn (-1) noAnnList) [lmatch])

--
-- Typeclass
--

cxEmpty :: HsContext GhcPs
cxEmpty = []

cxTuple :: [HsType GhcPs] -> HsContext GhcPs
cxTuple typs =
  case typs of
    [] -> []
    (x : []) -> [mkL (-1) x]
    _ ->
      let typs' = init typs
          lastTyp = last typs
          typs'' =
            fmap
              (L (mkRelSrcSpanAnn (-1) (AnnListItem [AddCommaAnn (mkEpaDelta (-1))])))
              typs'
       in (typs'' ++ [mkL (-1) lastTyp])

classA :: String -> [HsType GhcPs] -> HsType GhcPs
classA name typs = foldl' tyapp (tycon name) typs'
  where
    typs' = fmap tyParen typs

mkInstance ::
  -- | Context
  HsContext GhcPs ->
  -- | Typeclass name
  String ->
  -- | instance types
  [HsType GhcPs] ->
  -- | instance definitions
  [HsBind GhcPs] ->
  -- | resultant declaration
  ClsInstDecl GhcPs
mkInstance ctxt name typs bnds =
  ClsInstDecl
    { cid_ext = ann,
      cid_poly_ty =
        mkL (-1) (HsSig noExtField (HsOuterImplicit noExtField) (mkL (-1) typcls)),
      cid_binds = bnds',
      cid_sigs = [],
      cid_tyfam_insts = [],
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
    annCtxt =
      AnnContext
        { ac_darrow = Just (NormalSyntax, mkEpaDelta 0),
          ac_open = [mkEpaDelta (-1)],
          ac_close = [mkEpaDelta (-1)]
        }
    insttyp = foldl' f (tycon name) typs
      where
        f acc x = tyapp acc (tyParen x)

instD :: ClsInstDecl GhcPs -> HsDecl GhcPs
instD = InstD noExtField . ClsInstD noExtField

--
-- Pattern
--

mkPVar :: String -> Pat GhcPs
mkPVar name = VarPat noExtField (mkLIdP (-1) name)

pApp :: String -> [Pat GhcPs] -> Pat GhcPs
pApp name pats =
  ConPat
    { pat_con_ext = mkRelEpAnn (-1) [],
      pat_con = mkLIdP (-1) name,
      pat_args = PrefixCon [] lpats
    }
  where
    lpats = fmap (mkL (-1)) pats

--
-- Expr
--

app :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
app x y =
  HsApp (mkRelEpAnn (-1) NoEpAnns) lx ly
  where
    lx = mkL (-1) x
    ly = mkL 0 y

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
          litms = fmap (mkL (-1)) itms
       in ExplicitList (mkRelEpAnn (-1) ann) litms
  where

mkVar :: String -> HsExpr GhcPs
mkVar name =
  HsVar noExtField (mkLIdP (-1) name)

op :: String -> HsExpr GhcPs
op = mkVar

par :: HsExpr GhcPs -> HsExpr GhcPs
par expr =
  HsPar ann tokOpen (mkL (-1) expr) tokClose
  where
    ann = mkRelEpAnn (-1) NoEpAnns
    tokOpen = L (tokLoc (-1)) HsTok
    tokClose = L (tokLoc (-1)) HsTok

strE :: String -> HsExpr GhcPs
strE str = HsLit ann1 (HsString ann2 (fromString str))
  where
    str' = show str
    ann1 = mkRelEpAnn (-1) NoEpAnns
    ann2 = SourceText str'

--
-- Statements
--

mkBodyStmt :: HsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkBodyStmt expr =
  BodyStmt noExtField body noExtField noExtField
  where
    body = mkL (-1) expr

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

tyForeignPtr :: Type ()
tyForeignPtr = tycon "ForeignPtr"

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

generator :: Pat () -> Exp () -> Stmt ()
generator = Generator ()

qualifier :: Exp () -> Stmt ()
qualifier = Qualifier ()

clsDecl :: Decl () -> ClassDecl ()
clsDecl = ClsDecl ()

unkindedVar :: Name () -> TyVarBind ()
unkindedVar = UnkindedVar ()

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
