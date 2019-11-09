module TestTH where

import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Syntax as TH

test :: Q [Dec]
test = do
  TH.addModFinalizer $ TH.addForeignSource TH.LangCxx
    "#include <MacroPatternMatch.h>\n\
    \#include <vector>\n\
    \#include <string>\n\
    \#include \"Vector.h\"\n\
    \#include \"stdcxxType.h\"\n\
    \using namespace std;\n\
    \Vector_instance_s(int)\n\
    \Vector_instance(string)\n"
  pure []
