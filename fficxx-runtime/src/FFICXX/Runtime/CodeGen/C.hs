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

data CStatement =
    Include HeaderName       -- ^ #include "<header>"
  | UsingNamespace Namespace -- ^ using namespace <namespace>;
  | Pragma PragmaParam       -- ^ #pragma
  | EmptyLine                -- ^ just for convenience
  | Verbatim String          -- ^ temporary verbatim

data CBlock = ExternC [CStatement]

renderPragmaParam :: PragmaParam -> String
renderPragmaParam Once = "once"

render :: CStatement -> String
render (Include (HdrName hdr))  = "#include \"" <> hdr <> "\"\n"
render (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
render (Pragma param)           = "#pragma " <> renderPragmaParam param <> "\n"
render EmptyLine                = "\n"
render (Verbatim str)           = str

renderBlock :: CBlock -> String
renderBlock (ExternC cstmts) =
     "#ifdef __cplusplus\n\
     \extern \"C\" {\n\
     \#endif\n"
  ++ concatMap render cstmts
  ++ "#ifdef __cplusplus\n\
     \}\n\
     \#endif\n"
