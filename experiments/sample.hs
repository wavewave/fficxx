{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module MyModule where

instance Test F where
  f True = 1
  f False = 0

test = test1 {- abc -}

test2 = test3
