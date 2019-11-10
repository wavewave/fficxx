module FFICXX.Generate.Util.C where

import Data.Semigroup ( (<>) )
import FFICXX.Runtime.CodeGen.C ( CStatement(..) )
--
import FFICXX.Generate.Type.PackageInterface
       ( HeaderName(..), Namespace (..) )


include :: HeaderName -> CStatement
include (HdrName h) = Include h -- "#include \"" <> h <> "\""

usingNamespace :: Namespace -> CStatement
usingNamespace (NS ns) = UsingNamespace ns -- "using namespace " <> ns <> ";"
