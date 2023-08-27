module FFICXX.Runtime.Types
  ( Safety (..),
  )
where

data Safety = Unsafe | Safe | Interruptible
  deriving (Show)
