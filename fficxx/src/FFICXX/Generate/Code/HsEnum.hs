module FFICXX.Generate.Code.HsEnum
  ( genHsEnumInclude,
    genHsEnumDecl,
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
    app,
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
    mkVarWithComment,
    tycon,
    wildcard,
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
    --
    mk1to1 (x, y) = ([mkPVar x], mkVar y, EmptyLocalBinds noExtField)
    mkFromIntegralCxx x =
      mkVar "fromIntegral"
        `app` mkVarWithComment "" ("#{const " <> x <> "}")
    -- NOTE: toEnum should not be used.
    -- TODO: make this somewhat safer with error messages.
    mkToEnum = [([wildcard], mkVar "undefined", EmptyLocalBinds noExtField)]
    mkFromEnum x =
      ( [mkPVar x],
        mkFromIntegralCxx x,
        EmptyLocalBinds noExtField
      )
    bnds =
      [ mkBind (DifferentLine 1 2) "succ" (fmap mk1to1 (zip cnstrs (tail cnstrs))),
        mkBind (DifferentLine 1 2) "pred" (fmap mk1to1 (zip (tail cnstrs) cnstrs)),
        mkBind (DifferentLine 1 2) "toEnum" mkToEnum,
        mkBind (DifferentLine 1 2) "fromEnum" (fmap mkFromEnum cnstrs)
      ]
    enumInstDecl = mkInstance cxEmpty "Enum" [tycon typ] [] bnds
