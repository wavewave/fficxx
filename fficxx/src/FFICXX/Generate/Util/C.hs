module FFICXX.Generate.Util.C where

import Data.Semigroup ( (<>) )
import FFICXX.Generate.Type.PackageInterface ( HeaderName(..)
                                             , Namespace (..)
                                             )

include :: HeaderName -> String
include (HdrName h) = "#include \"" <> h <> "\""

usingNamespace :: Namespace -> String
usingNamespace (NS ns) = "using namespace " <> ns <> ";"
