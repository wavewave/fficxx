module HROOT.Generate.FFI where


import HROOT.Generate.Util 
import HROOT.Generate.Function
import HROOT.Generate.Class
import HROOT.Generate.Templates

ffistub :: String
ffistub = "foreign import ccall \"$headerfilename$ $classname$_$funcname$\" $hsfuncname$ \n  :: $hsargs$"


hsFFIClassFunc :: Class -> Function -> String 
hsFFIClassFunc c f 
  | isNewFunc f    = render ffistub 
                            [ ("headerfilename",headerFileName) 
                            , ("classname",class_name c)
                            , ("funcname", aliasedFuncName c f)
                            , ("hsfuncname",hscFuncName c f)
                            , ("hsargs", hsFuncTypNoSelf c f) ] 
  | isDeleteFunc f = "" 
  | otherwise      = render ffistub  
                            [ ("headerfilename",headerFileName) 
                            , ("classname",class_name c)
                            , ("funcname", aliasedFuncName c f)
                            , ("hsfuncname",hscFuncName c f)
                            , ("hsargs", hsFuncTyp c f) ] 

hsFFIClass :: Class -> String 
hsFFIClass c =
  let allfns = concatMap (virtualFuncs . class_funcs) 
                         (class_allparents c)
               ++ (class_funcs c) 
  in  intercalateWith connRet (hsFFIClassFunc c) allfns  

mkFFIClasses :: [Class] -> String 
mkFFIClasses = intercalateWith connRet2 hsFFIClass 


