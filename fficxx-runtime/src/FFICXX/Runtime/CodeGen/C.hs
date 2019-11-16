{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Runtime.CodeGen.C where

import Data.Hashable  ( Hashable )
import Data.Semigroup ( (<>) )
import Data.String    ( IsString(..) )


newtype HeaderName =
  HdrName { unHdrName :: String }
  deriving (Hashable, Show, Eq, Ord)


instance IsString HeaderName where
  fromString = HdrName

newtype Namespace =
  NS { unNamespace :: String }
  deriving (Show,Eq,Ord)

instance IsString Namespace where
  fromString = NS

data PragmaParam = Once

data CType = CType String

data Name = Name String

data CStatement =
    Include HeaderName       -- ^ #include "<header>"
  | UsingNamespace Namespace -- ^ using namespace <namespace>;
  | Pragma PragmaParam       -- ^ #pragma
  | TypeDef CType Name       -- ^ typedef origtype newname;
  | EmptyLine                -- ^ just for convenience
  | Comment String           -- ^ comment
  | Verbatim String          -- ^ temporary verbatim

data CBlock = ExternC [CStatement]

renderPragmaParam :: PragmaParam -> String
renderPragmaParam Once = "once"

render :: CStatement -> String
render (Include (HdrName hdr))  = "\n#include \"" <> hdr <> "\"\n"
render (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
render (Pragma param)           = "\n#pragma " <> renderPragmaParam param <> "\n"
render (TypeDef (CType typ) (Name n)) = "typedef " <> typ <> " " <> n <> ";"
render EmptyLine                = "\n"
render (Comment str)            = "// " ++ str ++ "\n"
render (Verbatim str)           = str

renderBlock :: CBlock -> String
renderBlock (ExternC cstmts) =
     "\n#ifdef __cplusplus\n\
     \extern \"C\" {\n\
     \#endif\n"
  ++ concatMap render cstmts
  ++ "\n#ifdef __cplusplus\n\
     \}\n\
     \#endif\n"
