module FFICXX.Generate.QQ.Verbatim where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote

verbatim :: QuasiQuoter
verbatim =
  QuasiQuoter
    { quoteExp = litE . stringL,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
      --           , quotePat = litP . stringP
    }
