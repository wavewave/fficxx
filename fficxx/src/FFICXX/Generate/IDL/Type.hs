{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Generate.IDL.Type
  ( Name (..),
    Typ (..),
    Arg (..),
    Function (..),
  )
where

import Data.Text (Text)

newtype Name = Name Text
  deriving (Show)

newtype Typ = Typ Name
  deriving (Show)

data Arg = Arg Typ Name
  deriving (Show)

data Function = Function Typ Name [Arg]
  deriving (Show)
