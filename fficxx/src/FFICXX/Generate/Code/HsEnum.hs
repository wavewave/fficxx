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
import FFICXX.Generate.Type.Class (EnumType)
import FFICXX.Generate.Util.GHCExactPrint
  ( comment,
    conDecl,
    mkData,
  )
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax
  ( HsDecl (DocD, TyClD),
    noExtField,
  )

genHsEnumInclude :: EnumType -> HsDecl GhcPs
genHsEnumInclude _ = DocD noExtField (comment "#include \"hello\"")

--  DocD noExtField (docCommentNext "hello there")

genHsEnumDecl :: EnumType -> HsDecl GhcPs
genHsEnumDecl enum =
  TyClD noExtField $ mkData typ [] cnstrExps []
  where
    typ = enumDataTypeName enum
    cnstrs = enumDataConstructorNames enum
    cnstrExps =
      fmap (\n -> conDecl n []) cnstrs

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
