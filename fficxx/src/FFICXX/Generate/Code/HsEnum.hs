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
import FFICXX.Generate.Util.HaskellSrcExts
  ( conDecl,
    mkData,
    qualConDecl,
  )
import Language.Haskell.Exts.Syntax (Decl (..))

genHsEnumDecl :: EnumType -> Reader AnnotateMap (Decl ())
genHsEnumDecl enum = pure expr
  where
    typ = enumDataTypeName enum
    cnstrs = enumDataConstructorNames enum
    cnstrExps =
      fmap (\n -> qualConDecl Nothing Nothing (conDecl n [])) cnstrs
    expr = mkData typ [] cnstrExps Nothing
