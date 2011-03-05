module Util where

import CType 

conn st x y = x ++ st ++ y  

connspace = conn " " 
conncomma =  conn "," 
connBSlash = conn "\\\n"
connSemicolonBSlash = conn "; \\\n"
connRet = conn "\n"
connRet2 = conn "\n\n"

intercalateWith  f mapper x 
  | not (null x) = (foldl1 (\x y -> x `f` y) . (map mapper)) x  
  | otherwise    = "" 


cvarToStr ctyp isconst varname = (ctypToStr ctyp isconst) `connspace` varname 

ctypToStr ctyp isconst = 
  let typword = case ctyp of 
        CTString -> "char *"
        CTInt    -> "int " 
        CTDouble -> "double" 
        CTBool   -> "bool"
  in case isconst of 
        Const   -> "const" `connspace` typword 
        NoConst -> typword 