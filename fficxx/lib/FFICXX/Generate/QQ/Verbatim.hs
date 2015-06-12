-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.QQ.Verbatim
-- Copyright   : (c) 2011-2013,2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.QQ.Verbatim where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

verbatim :: QuasiQuoter
verbatim = QuasiQuoter 
           { quoteExp = litE . stringL
           , quotePat = fail
           , quoteType = fail
           , quoteDec = fail
--           , quotePat = litP . stringP
           } 

