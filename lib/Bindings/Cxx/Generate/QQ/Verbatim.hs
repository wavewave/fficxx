module Bindings.Cxx.Generate.QQ.Verbatim where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

verbatim :: QuasiQuoter
verbatim = QuasiQuoter { 
             quoteExp = litE . stringL
--           , quotePat = litP . stringP
           } 

