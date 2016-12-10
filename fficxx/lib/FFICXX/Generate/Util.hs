-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Util
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
import           Data.Text               (Text)
import qualified Data.Text          as T
import           Data.Text.Template
-- import qualified Text.StringTemplate as ST

moduleDirFile :: String -> (String,String)
moduleDirFile mname = 
  let splitted = splitOn "." mname
      moddir  = intercalate "/" (init splitted )
      modfile = (last splitted) ++ ".hs" 
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
conn st x y = x ++ st ++ y  

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


context :: [(Text,String)] -> Context
context assocs x = maybe err (T.pack) . lookup x $ assocs
  where err = error $ "Could not find key: " ++ (T.unpack x)

        
-- render :: String -> [(String,String)] -> String        
-- render tmpl attribs = (ST.render . ST.setManyAttrib attribs . ST.newSTMP) tmpl 

{- 
renderTemplateGroup :: (ST.ToSElem a) => ST.STGroup String -> [(String,a)] 
                    -> [Char] -> String 
renderTemplateGroup gr attrs tmpl = 
    maybe ("template not found: " ++ tmpl)
          (ST.toString . setManyAttribSafer attrs) 
          (ST.getStringTemplate tmpl gr)
 
setManyAttribSafer :: (ST.Stringable b, ST.ToSElem a) => 
                      [(String, a)] 
                   -> ST.StringTemplate b 
                   -> ST.StringTemplate b
setManyAttribSafer attrs st = 
    let mbFoundbadattr = find badTmplVarName . map fst $ attrs 
    in maybe (ST.setManyAttrib attrs st) 
             (\mbA -> ST.newSTMP . ("setManyAttribSafer, bad template atr: "++) 
                      $ mbA)
             mbFoundbadattr 
  where badTmplVarName :: String -> Bool 
        badTmplVarName t = not . null . filter (not . isAlpha) $ t 

-}
