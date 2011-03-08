module HsCode where

import Text.StringTemplate hiding (render)

import qualified Data.Map as M

import CType
import Util
import Function
import Class
import Templates

----------                        

hsClassDeclHeaderTmpl = "class $classname$ a where"

hsClassDeclFuncTmpl = "    $funcname$ :: $args$ "

  
classToHsDecl :: Class -> String 
classToHsDecl c | length (class_funcs c) <= 0 = ""
classToHsDecl c | length (class_funcs c) > 0 =  
  let header = render hsClassDeclHeaderTmpl [ ("classname", class_name c ) ] 
      bodyline func = render hsClassDeclFuncTmpl 
                             [ ("funcname", hsFuncName func) 
                             , ("args" , argstr func ) 
                             ] 
      argstr func = intercalateWith connArrow id $
                                    [ "a" ] 
                                    ++ map (ctypeToHsType c.fst) (func_args func)
                                    ++ ["IO " ++ (ctypeToHsType c.func_ret) func ]
      bodylines = map bodyline (class_funcs c) 
  in  intercalateWith connRet id (header : bodylines) 

hsArgs :: Class -> Args -> String
hsArgs c = intercalateWith connArrow (ctypeToHsType c. fst) 


classesToHsDecls :: [Class] -> String 
classesToHsDecls = intercalateWith connRet2 classToHsDecl 


hsClassMethodExport :: Class    -- ^ only concrete class
                       -> String 
hsClassMethodExport c 
  | (not.null) exportFuncs  =                        
    let expFuncName f = case func_export f of 
                               FullName -> firstLower (class_name c) ++ func_name f   
                               Alias str -> str   
        header f = (expFuncName f) ++ " :: " ++ argstr f
        body f  = (expFuncName f)  ++ " = " ++ hsFuncXformer f ++ " " ++ hscFuncName c f 
        argstr func = intercalateWith connArrow id $ [class_name c]  
                                                     ++ map (ctypeToHsType c.fst) (func_args func)
                                                     ++ ["IO " ++ (ctypeToHsType c.func_ret) func ] 
    in  intercalateWith connRet (\f -> header f ++ "\n" ++ body f) exportFuncs
  | otherwise = ""   
 where exportFuncs = filter isExportFunc (class_funcs c)

                       

classToHsDefNew :: Class         -- ^ only concrete class 
                    -> String 
classToHsDefNew c = 
  if null newfuncs 
  then ""
  else let newfunc = head newfuncs
           newlinehead = "new" ++ class_name c ++ " :: " ++ argstr newfunc 
           newlinebody = "new" ++ class_name c ++ " = " ++ hsFuncXformerNew newfunc ++ " " ++ hscFuncName c newfunc 
           argstr func = intercalateWith connArrow id $
                                               map (ctypeToHsType c.fst) (func_args func)
                                               ++ ["IO " ++ (ctypeToHsType c.func_ret) func ]
           newline = newlinehead ++ "\n" ++ newlinebody 
       in newline
  where newfuncs = filter isNewFunc (class_funcs c)  

classesToHsDefNews :: [Class]    -- ^ only concrete class 
                      -> String 
classesToHsDefNews = intercalateWith connRet2 classToHsDefNew 
