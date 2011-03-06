module Util where

import CType 

import Data.Char 
import Text.StringTemplate.Helpers

toUppers = map toUpper
toLowers = map toLower

conn st x y = x ++ st ++ y  

connspace = conn " " 
conncomma =  conn ", " 
connBSlash = conn "\\\n"
connSemicolonBSlash = conn "; \\\n"
connRet = conn "\n"
connRet2 = conn "\n\n"
connArrow = conn " -> " 


intercalateWith  f mapper x 
  | not (null x) = (foldl1 (\x y -> x `f` y) . (map mapper)) x  
  | otherwise    = "" 


cvarToStr ctyp isconst varname = (ctypToStr ctyp isconst) `connspace` varname 

ctypToStr ctyp isconst = 
  let typword = case ctyp of 
        CTString -> "char *"
        CTInt    -> "int " 
        CTDouble -> "double" 
        CTBool   -> "int"              -- Currently available solution
        CTDoubleStar -> "double *"
  in case isconst of 
        Const   -> "const" `connspace` typword 
        NoConst -> typword 
        
        
render = flip render1 