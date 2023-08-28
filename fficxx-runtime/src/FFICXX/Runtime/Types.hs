module FFICXX.Runtime.Types
  ( FFISafety (..),
  )
where

data FFISafety = FFIUnsafe | FFISafe | FFIInterruptible
  deriving (Show)
