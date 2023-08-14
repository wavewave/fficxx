module FFICXX.Generate.Code.HsEnum
  ( genHsEnumInclude,
    genHsEnumDecl,
    -- genHsEnumFFI,
  )
where

import Control.Monad.Reader (Reader)
import FFICXX.Generate.Name
  ( enumDataConstructorNames,
    enumDataTypeName,
  )
import FFICXX.Generate.Type.Annotate (AnnotateMap)
import FFICXX.Generate.Type.Class (EnumType (..))
import FFICXX.Generate.Util.GHCExactPrint
  ( DeclGroup,
    comment,
    conDecl,
    cxEmpty,
    instD,
    mkBind,
    mkData,
    mkDeriving,
    mkInstance,
    mkPVar,
    mkVar,
    tycon,
  )
import GHC.Hs (GhcPs)
import GHC.Parser.Annotation (DeltaPos (..))
import Language.Haskell.Syntax
  ( HsDecl (DocD, TyClD),
    HsLocalBindsLR (..),
    noExtField,
  )

genHsEnumInclude :: EnumType -> DeclGroup
genHsEnumInclude enum =
  comment incstr
  where
    incstr = "#include \"" <> enum_header enum <> "\""

genHsEnumDecl :: EnumType -> [HsDecl GhcPs]
genHsEnumDecl enum =
  [ TyClD noExtField datDecl,
    instD enumInstDecl
  ]
  where
    typ = enumDataTypeName enum
    cnstrs = enumDataConstructorNames enum
    cnstrExps =
      fmap (\n -> conDecl n []) cnstrs
    deriv =
      mkDeriving [tycon "Eq", tycon "Ord", tycon "Show"]
    datDecl = mkData typ [] cnstrExps deriv

    mk1to1 (x, y) = ([mkPVar x], mkVar y, EmptyLocalBinds noExtField)

    bnds =
      [ mkBind (DifferentLine 1 2) "succ" (fmap mk1to1 (zip cnstrs (tail cnstrs))),
        mkBind (DifferentLine 1 2) "pred" (fmap mk1to1 (zip (tail cnstrs) cnstrs)),
        mkBind (DifferentLine 1 2) "toEnum" [],
        mkBind (DifferentLine 1 2) "fromEnum" []
      ]
    enumInstDecl = mkInstance cxEmpty "Enum" [tycon typ] [] bnds

{-
genHsEnumFFI :: EnumType -> HsDecl GhcPs
genHsEnumFFI enum = decl
  where
    typ = enumDataTypeName enum
    cnstrs = enumDataConstructorNames enum
    cnstrExps =
      fmap (\n -> conDecl n []) cnstrs
    decl = TyClD noExtField $ mkData typ [] cnstrExps []
-}
