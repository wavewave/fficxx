module Bindings.Cxx.Generate.Code.HsFFI where


import Bindings.Cxx.Generate.Util 
-- import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.Class
-- import Bindings.Cxx.Generate.Generator.Templates

genHsFFI :: ClassImportHeader -> String 
genHsFFI header =
  let c = cihClass header
      h = cihSelfHeader header
      allfns = concatMap (virtualFuncs . class_funcs) 
                         (class_allparents c)
               ++ (class_funcs c) 
  in  intercalateWith connRet (hsFFIClassFunc h c) allfns  

genAllHsFFI :: [ClassImportHeader] -> String 
genAllHsFFI = intercalateWith connRet2 genHsFFI 

--------

ffistub :: String
ffistub = "foreign import ccall \"$headerfilename$ $classname$_$funcname$\" $hsfuncname$ \n  :: $hsargs$"


hsFFIClassFunc :: FilePath -> Class -> Function -> String 
hsFFIClassFunc headerfilename c f = if isAbstractClass c 
                       then ""
                       else if (isNewFunc f || isStaticFunc f)
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

