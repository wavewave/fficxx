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

rawToHighStub = "data $rawname$\nnewtype $highname$ = $highname$ (ForeignPtr $rawname$) deriving (Eq, Ord, Show)\n"

hsFFIClassFunc :: Class -> Function -> String 
hsFFIClassFunc c f = render ffistub  
                            [ ("headerfilename",headerFileName) 
                            , ("classname",class_name c)
                            , ("funcname", func_name f)
                            , ("hsfuncname",hscFuncName c f)
                            , ("hsargs", hsFuncTyp c f) ] 

hsFFIClass :: Class -> String 
hsFFIClass c = let allfns = concatMap class_funcs (class_parents c) 
               in  intercalateWith connRet (hsFFIClassFunc c) allfns  

mkFFIClasses :: [Class] -> String 
mkFFIClasses = intercalateWith connRet2 hsFFIClass 


hsClassType :: Class -> String 
hsClassType c = render rawToHighStub [ ("rawname",rawname), ("highname",highname) ]
  where (highname,rawname) = hsClassName c
            

