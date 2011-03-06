module Class where

import Data.Char

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import qualified Data.Map as M

import CType
import Util
import Function
import Templates

data Class = Class { 
    class_name :: String, 
    class_parents :: [Class],
    class_funcs :: [Function]
  } 

instance Show Class where
  show x = show (class_name x)

instance Eq Class where
  (==) x y = class_name x == class_name y

instance Ord Class where
  compare x y = compare (class_name x) (class_name y)

type DaughterMap = M.Map Class [Class] 

mkDaughterMap :: [Class] -> DaughterMap 
mkDaughterMap = foldl mkDaughterMapWorker M.empty  
  where mkDaughterMapWorker m c = let ps = class_parents c 
                                  in  foldl (addmeToYourDaughterList c) m ps 
        addmeToYourDaughterList c m p = let f Nothing = Just [c]
                                            f (Just cs)  = Just (c:cs)    
                                        in  M.alter f p m
       
-- Class Declaration and Definition


classToDeclDef :: Class -> String 
classToDeclDef aclass =  
  let tmpl = "#undef ROOT_$classname$_DECLARATION\\\n#define ROOT_$classname$_DECLARATION(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = funcsToDecls (class_funcs aclass)
  in  declBodyStr 
      

classesToDeclsDef :: [Class] -> String 
classesToDeclsDef = intercalateWith connRet2 classToDeclDef

-----

classToTypeDecl :: Class -> String 
classToTypeDecl c = let tmpl = "ROOT_TYPE_DECLARATION($classname$);" 
                    in  render tmpl [ ("classname", class_name c) ] 

classesToTypeDecls = intercalateWith connRet (classToTypeDecl ) 

-----

classesToClassDecls :: DaughterMap -> String 
classesToClassDecls  m = 
  let lst = M.toList m 
      f (x,ys) = let strx = map toUpper (class_name x) 
                 in  concatMap (\y ->"ROOT_"++strx++"_DECLARATION(" ++ class_name y ++ ");\n") ys
  in  concatMap f lst


-----

classToDef :: Class -> String 
classToDef aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITION\\\n#define ROOT_$classname$_DEFINITION(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = funcsToDefs (class_funcs aclass)
  in  defBodyStr 
      
classesToDefs = intercalateWith connRet2 classToDef

  
----

hsClassName :: Class 
               -> (String, String)  -- ^ High-level, 'Raw'-level
hsClassName c = 
  let cname = class_name c
  in (cname, "Raw" ++ cname) 


hsFuncTyp :: Class -> Function -> String
hsFuncTyp c f = let args = func_args f 
                    ret  = func_ret  f 
                in  self ++ " -> " ++ concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret 
                    
  where (hcname,rcname) = hsClassName c
        self = "(Ptr " ++ rcname ++ ")" 

        hsargtype (CT ctype _) = hsCTypeName ctype
        hsargtype (CPT x _) = hsCppTypeName x 
        hsargtype SelfType = self 
        
        hsrettype Void = "IO ()"
        hsrettype SelfType = "IO " ++ self
        hsrettype (CT ctype _) = "IO " ++ hsCTypeName ctype
        hsrettype (CPT x _ ) = "IO " ++ hsCppTypeName x 
        
hscFuncName :: Class -> Function -> String         
hscFuncName c f = "c_" ++ toLowers (class_name c) ++ "_" ++ toLowers (func_name f)
        
hsFuncName :: Function -> String 
hsFuncName f = let (x:xs) = func_name f 
               in (toLower x) : xs
                  
hsFuncXformer :: Function -> String 
hsFuncXformer func = let len = length (func_args func) 
                     in "xform" ++ show len
                        
                        
                        
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
                                    ++ map (ctypeToHsType.fst) (func_args func)
                                    ++ ["IO " ++ (ctypeToHsType.func_ret) func ]
      bodylines = map bodyline (class_funcs c) 
  in  intercalateWith connRet id (header : bodylines) 

hsArgs :: Args -> String
hsArgs = intercalateWith connArrow (ctypeToHsType . fst) 


classesToHsDecls :: [Class] -> String 
classesToHsDecls = intercalateWith connRet2 classToHsDecl 