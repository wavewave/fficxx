{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Util
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------


module FFICXX.Generate.Util where

import           Data.Char 
import           Data.List
import           Data.List.Split
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import           Data.Text.Template


moduleDirFile :: String -> (String,String)
moduleDirFile mname = 
  let splitted = splitOn "." mname
      moddir  = intercalate "/" (init splitted )
      modfile = (last splitted) <> ".hs" 
  in  (moddir, modfile)

hline :: IO ()
hline = putStrLn "--------------------------------------------------------"

toUppers :: String -> String
toUppers = map toUpper

toLowers :: String -> String 
toLowers = map toLower


firstLower :: String -> String 
firstLower [] = [] 
firstLower (x:xs) = (toLower x) : xs 

conn :: String -> String -> String -> String 
conn st x y = x <> st <> y  

connspace :: String -> String -> String
connspace = conn " " 

conncomma :: String -> String -> String
conncomma =  conn ", " 

connBSlash :: String -> String -> String 
connBSlash = conn "\\\n"

connSemicolonBSlash :: String -> String -> String
connSemicolonBSlash = conn "; \\\n"

connRet :: String -> String -> String
connRet = conn "\n"

connRet2 :: String -> String -> String 
connRet2 = conn "\n\n"

connArrow :: String -> String -> String 
connArrow = conn " -> " 

intercalateWith :: (String-> String -> String) -> (a->String) -> [a] -> String
intercalateWith  f mapper x 
  | not (null x) = foldl1 f (map mapper x)
  | otherwise    = "" 


intercalateWithM :: (Monad m) => (String -> String -> String) -> (a->m String) -> [a] -> m String 
intercalateWithM f mapper x 
  | not (null x) = do ms <- mapM mapper x
                      return (foldl1 f ms)
  | otherwise = return "" 


-- TODO: deprecate this and use contextT
context :: [(Text,String)] -> Context
context assocs x = maybe err (T.pack) . lookup x $ assocs
  where err = error $ "Could not find key: " <> (T.unpack x)

-- TODO: Rename this to context.
-- TODO: Proper error handling.
contextT :: [(Text,Text)] -> Context
contextT assocs x = fromMaybe err . lookup x $ assocs
  where err = error $ T.unpack ("Could not find key: " <> x)

subst :: Text -> Context -> String
subst t c = TL.unpack (substitute t c)

