{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.IDL.Parser
  ( idl,
  )
where

-- import Control.Applicative (many)
import Data.Attoparsec.Text
  ( Parser,
    char,
    letter,
    many1,
    sepBy,
    skipSpace,
  )
import qualified Data.Text as T
import FFICXX.Generate.IDL.Type
  ( Arg (..),
    Function (..),
    Name (..),
    Typ (..),
  )

typ :: Parser Typ
typ = Typ <$> name

name :: Parser Name
name = Name . T.pack <$> many1 letter

arg :: Parser Arg
arg = do
  t <- typ
  skipSpace
  v <- name
  pure (Arg t v)

idl :: Parser Function
idl = do
  out <- typ
  skipSpace
  fname <- name
  skipSpace
  char '('
  skipSpace
  args <- arg `sepBy` (skipSpace >> char ',' >> skipSpace)
  char ')'
  skipSpace
  char ';'
  pure (Function out fname args)
