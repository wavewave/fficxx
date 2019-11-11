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

data CStatement =
    Include HeaderName       -- ^ #include "<header>"
  | UsingNamespace Namespace -- ^ using namespace <namespace>;
  | Verbatim String          -- ^ temporary verbatim

render :: CStatement -> String
render (Include (HdrName hdr)) = "#include \"" <> hdr <> "\"\n"
render (UsingNamespace (NS ns)) = "using namespace " <> ns <> ";"
render (Verbatim str) = str
