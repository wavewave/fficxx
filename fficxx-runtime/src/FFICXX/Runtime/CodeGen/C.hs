module FFICXX.Runtime.CodeGen.C where

import Data.Semigroup ( (<>) )

data CStatement =
    Include String        -- ^ #include "<header>"
  | UsingNamespace String -- ^ using namespace <namespace>;
  | Verbatim String       -- ^ temporary verbatim

render :: CStatement -> String
render (Include hdr) = "#include \"" <> hdr <> "\""
render (UsingNamespace ns) = "using namespace " <> ns <> ";"
render (Verbatim str) = str
