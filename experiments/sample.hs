{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -w #-}

module MyModule where

data K = K Int

{- test :: IO ()
test = do
  addModFinalizer (addForeignSource LangCxx "\n#include \"test\"")

instance (C a) => D (P a) (Q a) where
  dinst x = x * x
-}
