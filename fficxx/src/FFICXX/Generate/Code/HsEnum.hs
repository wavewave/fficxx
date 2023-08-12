module FFICXX.Generate.Code.HsEnum
  ( genHsEnumDecl,
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
  ( conDecl,
    mkData,
  )
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax
  ( HsDecl (TyClD),
    noExtField,
  )

genHsEnumDecl :: EnumType -> Reader AnnotateMap (HsDecl GhcPs)
genHsEnumDecl enum = pure expr
  where
    typ = enumDataTypeName enum
    cnstrs = enumDataConstructorNames enum
    cnstrExps =
      fmap (\n -> conDecl n []) cnstrs
    expr = TyClD noExtField $ mkData typ [] cnstrExps []
