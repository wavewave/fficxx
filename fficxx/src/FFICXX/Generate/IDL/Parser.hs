{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.IDL.Parser
  ( function,
    class_,
    package,
  )
where

import Control.Applicative (many, optional)
import Data.Attoparsec.Text
  ( Parser,
    char,
    many1,
    satisfy,
    sepBy,
    sepBy1,
    skipSpace,
    space,
    string,
  )
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import FFICXX.Generate.IDL.Type
  ( Arg (..),
    Class (..),
    Function (..),
    Name (..),
    Package (..),
    Typ (..),
  )

typ :: Parser Typ
typ = Typ <$> name

name :: Parser Name
name = Name . T.pack <$> many1 (satisfy isAlphaNum)

arg :: Parser Arg
arg = do
  t <- typ
  space
  skipSpace
  v <- name
  pure (Arg t v)

function :: Parser Function
function = do
  out <- typ
  space
  skipSpace
  fname <- name
  skipSpace
  char '('
  skipSpace
  args <- arg `sepBy` (skipSpace >> char ',' >> skipSpace)
  skipSpace
  char ')'
  skipSpace
  char ';'
  pure (Function out fname args)

class_ :: Parser Class
class_ = do
  string "class"
  space
  skipSpace
  cname <- name
  skipSpace
  parents <-
    fromMaybe []
      <$> optional
        ( do
            char ':'
            skipSpace
            ps <- name `sepBy1` (skipSpace >> char ',' >> skipSpace)
            skipSpace
            pure ps
        )
  char '{'
  skipSpace
  methods <- many function
  skipSpace
  char '}'
  skipSpace
  char ';'
  pure (Class cname parents methods)

package :: Parser Package
package = do
  fs <- many (skipSpace *> function <* skipSpace)
  cs <- many (skipSpace *> class_ <* skipSpace)
  pure (Package fs cs)
