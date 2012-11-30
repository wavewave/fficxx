module Bindings.Cxx.Generate.Util where


-- 
import           Data.Char 
import           Data.List
import           Data.List.Split 
import qualified Text.StringTemplate as ST
-- 
import           Bindings.Cxx.Generate.Type.CType 
-- 

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
render tmpl attribs = (ST.render . ST.setManyAttrib attribs . ST.newSTMP) tmpl 
   -- flip render1

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


