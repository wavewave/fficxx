module HsCode where

import qualified Data.Map as M

import CType
import Util
import Function
import Class

import Control.Monad.State
----------------

rawToHighDecl :: String
rawToHighDecl = "data $rawname$\nnewtype $highname$ = $highname$ (ForeignPtr $rawname$) deriving (Eq, Ord, Show)"

rawToHighInstance :: String
rawToHighInstance = "instance FPtr $highname$ where\n   type Raw $highname$ = $rawname$\n   get_fptr ($highname$ fptr) = fptr\n   cast_fptr_to_obj = $highname$"

{-
rawToHighInstanceCastable :: String
rawToHighInstanceCastable =  "instance Castable $highname$ (Ptr $rawname$) where\n  cast = unsafeForeignPtrToPtr.get_fptr\n  uncast x = cast_fptr_to_obj (unsafePerformIO (newForeignPtr_ x))"
-} 

hsClassType :: Class -> String 
hsClassType c = let decl = render rawToHighDecl tmplName
                    inst1 = render rawToHighInstance tmplName
               --      inst2 = render rawToHighInstanceCastable tmplName
                in  decl `connRet` inst1 {- `connRet` inst2 -}
  where (highname,rawname) = hsClassName c
        tmplName = [("rawname",rawname),("highname",highname)] 
            
mkRawClasses :: [Class] -> String 
mkRawClasses = intercalateWith connRet2 hsClassType


hsClassDeclHeaderTmpl :: String
hsClassDeclHeaderTmpl = "class $classname$ a where"

hsClassDeclFuncTmpl :: String
hsClassDeclFuncTmpl = "    $funcname$ :: $args$ "


mkHsFuncArgType :: Class -> Args -> ([String],[String]) 
mkHsFuncArgType c lst = 
  let  (args,state) = runState (mapM mkFuncArgTypeWorker lst) ([],0)
  in   (args,fst state)
  where mkFuncArgTypeWorker (typ,var) = 
          case typ of                  
            SelfType -> return "a"
            CT _ _   -> return $ ctypeToHsType c typ 
            CPT (CPTClass cname) _ -> do 
              (prefix,n) <- get 
              let iname = typeclassName c
                  newname = 'c' : show n
                  newprefix1 = iname ++ " " ++ newname    
                  newprefix2 = "FPtr " ++ newname
              put (newprefix1:newprefix2:prefix,n+1)
              return newname
           
      
      
classToHsDecl :: Class -> String 
classToHsDecl c | length (class_funcs c) <= 0 = ""
classToHsDecl c | length (class_funcs c) > 0 =  
  let header = render hsClassDeclHeaderTmpl [ ("classname", typeclassName c ) ] 
      bodyline func = render hsClassDeclFuncTmpl 
                                    [ ("funcname", hsFuncName func) 
                                    , ("args" , prefixstr func ++ argstr func ) 
                                    ] 
      prefixstr func =  
        let prefixlst = (snd . mkHsFuncArgType c . func_args) func
        in  if null prefixlst
              then "" 
              else "(" ++ (intercalateWith conncomma id prefixlst) ++ ") => "  
      argstr func = intercalateWith connArrow id $
                                    [ "a" ] 
                                    ++ fst (mkHsFuncArgType c (func_args func))
                                    {- ++ map (ctypeToHsType c.fst) (func_args func) -}
                                    ++ ["IO " ++ (ctypeToHsType c.func_ret) func ]
      bodylines = map bodyline . filter (\x -> (not.isNewFunc) x && (not.isExportFunc) x) 
                      $ (class_funcs c) 
  in  intercalateWith connRet id (header : bodylines) 

hsArgs :: Class -> Args -> String
hsArgs c = intercalateWith connArrow (ctypeToHsType c. fst) 


classesToHsDecls :: [Class] -> String 
classesToHsDecls = intercalateWith connRet2 classToHsDecl 


-----------------------------
-- classInstanceHeader = "instance $parent$ $daughter$ where" 

hsClassInstance :: Class -> Class -> String 
hsClassInstance parent child  = 
  let headline = "instance " ++ typeclassName parent ++ " " ++ class_name child ++ " where" 
      defline func = "  " ++ hsFuncName func ++ " = " ++ hsFuncXformer func ++ " " ++ hscFuncName child func 
      deflines = map defline (class_funcs parent) 
  in  intercalateWith connRet id (headline : deflines) 
   
mkClassInstances :: DaughterMap -> String 
mkClassInstances m = 
  let lst = M.toList m 
      f (x,ys) = intercalateWith connRet (hsClassInstance x) (x:ys)
  in  intercalateWith connRet2 f lst
      
      
-- hsClassDeclaration :: Class -> String
-- hsClassDeclaration c = undefined
  
----------                        

hsInterfaceCastableInstanceTmpl :: String 
hsInterfaceCastableInstanceTmpl = 
  "instance ($interfaceName$ a, FPtr a) => Castable a (Ptr $rawClassName$) where\n  cast = unsafeForeignPtrToPtr . castForeignPtr . get_fptr\n  uncast = cast_fptr_to_obj . castForeignPtr . unsafePerformIO . newForeignPtr_ \n"


hsInterfaceCastableInstance :: Class -> String 
hsInterfaceCastableInstance c = 
  let iname = typeclassName c
      (_,rname) = hsClassName c
  in  render hsInterfaceCastableInstanceTmpl 
             [("interfaceName",iname),("rawClassName",rname)]

mkInterfaceCastableInstance :: [Class] -> String 
mkInterfaceCastableInstance = 
  intercalateWith connRet2 hsInterfaceCastableInstance 


----------

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
