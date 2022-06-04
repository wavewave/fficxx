module FFICXX.Generate.QQ.Verbatim where

import Language.Haskell.TH.Lib
  ( litE,
    stringL,
  )
import Language.Haskell.TH.Quote
  ( QuasiQuoter (..),
    quoteDec,
    quoteExp,
    quotePat,
    quoteType,
  )

verbatim :: QuasiQuoter
verbatim =
  QuasiQuoter
    { quoteExp = litE . stringL,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
      --           , quotePat = litP . stringP
    }
