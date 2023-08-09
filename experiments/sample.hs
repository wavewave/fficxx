{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -w #-}

module MyModule where

data K = K Int

{- test :: IO ()
test = do
  addModFinalizer (addForeignSource LangCxx "\n#include \"test\"")

-}

instance (C a) => D (P a) (Q a) where
  type F (P a) = Double
  dinst x = x * x

newtype Loader = Loader (Ptr RawLoader) deriving (Eq,Ord,Show)
