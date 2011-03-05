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

rawToHighDecl = "data $rawname$\nnewtype $highname$ = $highname$ (ForeignPtr $rawname$) deriving (Eq, Ord, Show)"

rawToHighInstance = "instance FPtr $highname$ where\n   type Raw $highname$ = $rawname$\n   get_fptr ($highname$ fptr) = fptr\n   cast_fptr_to_obj = $highname$"


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
hsClassType c = let decl = render rawToHighDecl     [ ("rawname",rawname), ("highname",highname) ]
                    inst = render rawToHighInstance [ ("rawname",rawname), ("highname",highname) ]
                in  decl `connRet` inst 
  where (highname,rawname) = hsClassName c
            
mkHighLevelClasses :: [Class] -> String 
mkHighLevelClasses = intercalateWith connRet2 hsClassType

