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

rawToHighInstanceCastable =  "instance Castable $highname$ (Ptr $rawname$) where\n  cast = unsafeForeignPtrToPtr.get_fptr\n  uncast x = cast_fptr_to_obj (unsafePerformIO (newForeignPtr_ x))"




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


hsClassType :: Class -> String 
hsClassType c = let decl = render rawToHighDecl tmplName
                    inst1 = render rawToHighInstance tmplName
                    inst2 = render rawToHighInstanceCastable tmplName
                in  decl `connRet` inst1 `connRet` inst2
  where (highname,rawname) = hsClassName c
        tmplName = [("rawname",rawname),("highname",highname)] 
            
mkRawClasses :: [Class] -> String 
mkRawClasses = intercalateWith connRet2 hsClassType

-- classInstanceHeader = "instance $parent$ $daughter$ where" 

hsClassInstance :: Class -> Class -> String 
hsClassInstance parent child = 
  let headline = "instance " ++ class_name parent ++ " " ++ class_name child ++ " where" 
      defline func = "  " ++ hsFuncName func ++ " = " ++ hsFuncXformer func ++ " " ++ hscFuncName child func 
      deflines = map defline (class_funcs parent) 
  in  intercalateWith connRet id (headline : deflines) 
   
mkClassInstances :: DaughterMap -> String 
mkClassInstances m = 
  let lst = M.toList m 
      f (x,ys) = intercalateWith connRet (hsClassInstance x) ys
  in  intercalateWith connRet2 f lst
      
      
hsClassDeclaration :: Class -> String
hsClassDeclaration c = undefined
  