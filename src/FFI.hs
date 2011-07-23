module FFI where


import Util 
import Function
import Class
import Templates

ffistub :: String
ffistub = "foreign import ccall \"$headerfilename$ $classname$_$funcname$\" $hsfuncname$ \n  :: $hsargs$"





hsFFIClassFunc :: Class -> Function -> String 
hsFFIClassFunc c f = if isNewFunc f 
                     then render ffistub 
                                 [ ("headerfilename",headerFileName) 
                                 , ("classname",class_name c)
                                 , ("funcname", aliasedFuncName f)
                                 , ("hsfuncname",hscFuncName c f)
                                 , ("hsargs", hsFuncTypNoSelf c f) ] 

                                  
                     else render ffistub  
                                 [ ("headerfilename",headerFileName) 
                                 , ("classname",class_name c)
                                 , ("funcname", aliasedFuncName f)
                                 , ("hsfuncname",hscFuncName c f)
                                 , ("hsargs", hsFuncTyp c f) ] 

hsFFIClass :: Class -> String 
hsFFIClass c = let allfns = concatMap class_funcs (c : class_allparents c) 
               in  intercalateWith connRet (hsFFIClassFunc c) allfns  

mkFFIClasses :: [Class] -> String 
mkFFIClasses = intercalateWith connRet2 hsFFIClass 


