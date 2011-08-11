module HROOT.Generate.Code.HsFFI where


import HROOT.Generate.Util 
import HROOT.Generate.Type.Method
import HROOT.Generate.Type.Class
-- import HROOT.Generate.Generator.Templates

ffistub :: String
ffistub = "foreign import ccall \"$headerfilename$ $classname$_$funcname$\" $hsfuncname$ \n  :: $hsargs$"


hsFFIClassFunc :: FilePath -> Class -> Function -> String 
hsFFIClassFunc headerfilename c f = if isAbstractClass c 
                       then ""
                       else if isNewFunc f     
                              then render ffistub 
                                       [ ("headerfilename",headerfilename) 
                                       , ("classname",class_name c)
                                       , ("funcname", aliasedFuncName c f)
                                       , ("hsfuncname",hscFuncName c f)
                                       , ("hsargs", hsFuncTypNoSelf c f) ] 
                              else render ffistub 
                                       [ ("headerfilename",headerfilename) 
                                       , ("classname",class_name c)
                                       , ("funcname", aliasedFuncName c f)
                                       , ("hsfuncname",hscFuncName c f)
                                       , ("hsargs", hsFuncTyp c f) ] 
{-  | otherwise      = render ffistub  
                            [ ("headerfilename",headerFileName) 
                            , ("classname",class_name c)
                            , ("funcname", aliasedFuncName c f)
                            , ("hsfuncname",hscFuncName c f)
                            , ("hsargs", hsFuncTyp c f) ]  -}

hsFFIClass :: FilePath -> Class -> String 
hsFFIClass h c =
  let allfns = concatMap (virtualFuncs . class_funcs) 
                         (class_allparents c)
               ++ (class_funcs c) 
  in  intercalateWith connRet (hsFFIClassFunc h c) allfns  

mkFFIClasses :: FilePath -> [Class] -> String 
mkFFIClasses h = intercalateWith connRet2 (hsFFIClass h) 


