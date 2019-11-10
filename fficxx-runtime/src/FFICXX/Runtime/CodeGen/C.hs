{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Runtime.CodeGen.C where

import Data.Hashable  ( Hashable )
import Data.Semigroup ( (<>) )
import Data.String    ( IsString(..) )


newtype HeaderName = HdrName { unHdrName :: String }
                   deriving (Hashable, Show, Eq, Ord)


instance IsString HeaderName where
  fromString = HdrName

newtype Namespace = NS { unNamespace :: String } deriving (Show)


data CStatement =
    Include String        -- ^ #include "<header>"
  | UsingNamespace String -- ^ using namespace <namespace>;
  | Verbatim String       -- ^ temporary verbatim

render :: CStatement -> String
render (Include hdr) = "#include \"" <> hdr <> "\""
render (UsingNamespace ns) = "using namespace " <> ns <> ";"
render (Verbatim str) = str
