module FFI where

import Data.Char

import qualified Data.Map as M

import Util 
import Function
import Class
import Templates

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

ffistub = "foreign import ccall \"$headerfilename$ $classname$_$funcname$\" $hsfuncname$ \n  :: $hsargs$"





hsFFIClassFunc :: Class -> Function -> String 
hsFFIClassFunc c f = if isNewFunc f 
                     then render ffistub 
                                 [ ("headerfilename",headerFileName) 
                                 , ("classname",class_name c)
                                 , ("funcname", func_name f)
                                 , ("hsfuncname",hscFuncName c f)
                                 , ("hsargs", hsFuncTypNoSelf c f) ] 

                                  
                     else render ffistub  
                                 [ ("headerfilename",headerFileName) 
                                 , ("classname",class_name c)
                                 , ("funcname", func_name f)
                                 , ("hsfuncname",hscFuncName c f)
                                 , ("hsargs", hsFuncTyp c f) ] 

hsFFIClass :: Class -> String 
hsFFIClass c = let allfns = concatMap class_funcs (c : class_parents c) 
               in  intercalateWith connRet (hsFFIClassFunc c) allfns  

mkFFIClasses :: [Class] -> String 
mkFFIClasses = intercalateWith connRet2 hsFFIClass 


