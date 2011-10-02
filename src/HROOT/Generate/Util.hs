module HROOT.Generate.Util where

import HROOT.Generate.Type.CType 

import Data.Char 
import Text.StringTemplate.Helpers

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
-- (foldl1 (\x1 y -> x1 `f` y) . (map mapper)) x  
  | otherwise    = "" 


intercalateWithM :: (Monad m) => (String -> String -> String) -> (a->m String) -> [a] -> m String 
intercalateWithM f mapper x 
  | not (null x) = do ms <- mapM mapper x
                      return (foldl1 f ms)
  | otherwise = return "" 

-- intercalateM :: (Monad m) => String -> [String] -> m String 
-- intercalateM str = return . foldl1 (\x y-> x ++ str ++ y)


cvarToStr :: CTypes -> IsConst -> String -> String
cvarToStr ctyp isconst varname = (ctypToStr ctyp isconst) `connspace` varname 

ctypToStr :: CTypes -> IsConst -> String
ctypToStr ctyp isconst = 
  let typword = case ctyp of 
        CTString -> "char *"
        CTInt    -> "int " 
        CTUInt   -> "unsigned int "
        CTDouble -> "double" 
        CTBool   -> "int"              -- Currently available solution
        CTDoubleStar -> "double *"
        CTVoidStar -> "void *"
        CTIntStar -> "int *"
        CTCharStarStar -> "char **"
  in case isconst of 
        Const   -> "const" `connspace` typword 
        NoConst -> typword 
        
render :: String -> [(String,String)] -> String        
render = flip render1 