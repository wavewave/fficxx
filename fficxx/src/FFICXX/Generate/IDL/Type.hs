{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Generate.IDL.Type
  ( -- * elements
    Name (..),
    Typ (..),
    Arg (..),

    -- * high-level
    Function (..),
    Class (..),

    -- * top-level
    Package (..),
  )
where

import Data.Text (Text)

newtype Name = Name Text
  deriving (Show)

newtype Typ = Typ Name
  deriving (Show)

data Arg = Arg Typ Name
  deriving (Show)

data Function = Function
  { funcOutput :: Typ,
    funcName :: Name,
    funcArgs :: [Arg]
  }
  deriving (Show)

data Class = Class
  { className :: Name,
    classParents :: [Name],
    classMethods :: [Function]
  }
  deriving (Show)

data Package = Package
  { -- | top-level functions in the package
    pkgFunctions :: [Function],
    -- | classes
    pkgClasses :: [Class]
  }
  deriving (Show)
