module HROOT.Generate.Type.Class where

import Data.Char

import qualified Data.Map as M

import HROOT.Generate.Type.CType
import HROOT.Generate.Util
import HROOT.Generate.Type.Method
import Data.List 

data Class = Class { class_name :: String
                   , class_parents :: [Class]
                   , class_funcs :: [Function] }
           | AbstractClass { class_name :: String
                           , class_parents :: [Class]
                           , class_funcs :: [Function] }

data ClassImportHeader = ClassImportHeader
                       { cihClass :: Class 
                       , cihSelfHeader :: String 
                       , cihSelfCpp :: String
                       , cihIncludedCHeaders :: [String] 
                       }

data ClassModule = ClassModule 
                   { cmModule :: String
                   , cmClass :: [Class] 
                   , cmCIH :: [ClassImportHeader]                    
                   , cmImportedModules :: [String]
                   } 

data ClassGlobal = ClassGlobal 
                   { cgDaughterMap :: DaughterMap } 

-- | Check abstract clas

isAbstractClass :: Class -> Bool 
isAbstractClass (Class _ _ _) = False 
isAbstractClass (AbstractClass _ _ _ ) = True            

instance Show Class where
  show x = show (class_name x)

instance Eq Class where
  (==) x y = class_name x == class_name y

instance Ord Class where
  compare x y = compare (class_name x) (class_name y)

type DaughterMap = M.Map Class [Class] 

class_allparents :: Class -> [Class] 
class_allparents c = let ps = class_parents c
                     in  if null ps 
                           then []
                           else nub (ps ++ (concatMap class_allparents ps))


-- | Daughter map not including itself

mkDaughterMap :: [Class] -> DaughterMap 
mkDaughterMap = foldl mkDaughterMapWorker M.empty  
  where mkDaughterMapWorker m c = let ps = class_allparents c 
                                  in  foldl (addmeToYourDaughterList c) m ps 
        addmeToYourDaughterList c m p = let f Nothing = Just [c]
                                            f (Just cs)  = Just (c:cs)    
                                        in  M.alter f p m

-- | Daughter Map including itself as a daughter

mkDaughterSelfMap :: [Class] -> DaughterMap
mkDaughterSelfMap = foldl worker M.empty  
  where worker m c = let ps = c : class_allparents c 
                     in  foldl (addToList c) m ps 
        addToList c m p = let f Nothing = Just [c]
                              f (Just cs)  = Just (c:cs)    
                          in  M.alter f p m

       
ctypeToHsType :: Class -> Types -> String
ctypeToHsType _c Void = "()" 
ctypeToHsType c SelfType = class_name c
ctypeToHsType _c (CT CTString _) = "String"
ctypeToHsType _c (CT CTInt _) = "Int" 
ctypeToHsType _c (CT CTUInt _) = "Word"
ctypeToHsType _c (CT CTDouble _) = "Double"
ctypeToHsType _c (CT CTBool _ ) = "Int"
ctypeToHsType _c (CT CTDoubleStar _) = "[Double]"
ctypeToHsType _c (CT CTVoidStar _) = "(Ptr ())"
ctypeToHsType _c (CT CTIntStar _) = "[Int]" 
ctypeToHsType _c (CT CTCharStarStar _) = "[String]"
ctypeToHsType _c (CPT (CPTClass name) _) = name


typeclassName :: Class -> String
typeclassName c = 'I' : class_name c

typeclassNameFromStr :: String -> String 
typeclassNameFromStr = ('I':)

hsClassName :: Class 
               -> (String, String)  -- ^ High-level, 'Raw'-level
hsClassName c = 
  let cname = class_name c
  in (cname, "Raw" ++ cname) 

existConstructorName :: Class -> String 
existConstructorName c = 'E' : class_name c

hsFuncTyp :: Class -> Function -> String
hsFuncTyp c f = let args = genericFuncArgs f 
                    ret  = genericFuncRet f 
                in  self ++ " -> " ++ concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret 
                    
  where (_hcname,rcname) = hsClassName c
        self = "(Ptr " ++ rcname ++ ")" 

        hsargtype (CT ctype _) = hsCTypeName ctype
        hsargtype (CPT x _) = hsCppTypeName x 
        hsargtype SelfType = self 
        hsargtype _ = error "undefined hsargtype"
        
        hsrettype Void = "IO ()"
        hsrettype SelfType = "IO " ++ self
        hsrettype (CT ctype _) = "IO " ++ hsCTypeName ctype
        hsrettype (CPT x _ ) = "IO " ++ hsCppTypeName x 
        
hsFuncTypNoSelf :: Class -> Function -> String
hsFuncTypNoSelf c f = let args = genericFuncArgs f 
                          ret  = genericFuncRet f 
                      in  intercalateWith connArrow id $ map (hsargtype . fst) args ++ [hsrettype ret]  
                          
  where (_hcname,rcname) = hsClassName c
        self = "(Ptr " ++ rcname ++ ")" 

        hsargtype (CT ctype _) = hsCTypeName ctype
        hsargtype (CPT x _) = hsCppTypeName x 
        hsargtype SelfType = self 
        hsargtype _ = error "undefined hsargtype"
        
        hsrettype Void = "IO ()"
        hsrettype SelfType = "IO " ++ self
        hsrettype (CT ctype _) = "IO " ++ hsCTypeName ctype
        hsrettype (CPT x _ ) = "IO " ++ hsCppTypeName x 


hscFuncName :: Class -> Function -> String         
hscFuncName c f = "c_" ++ toLowers (class_name c) ++ "_" ++ toLowers (aliasedFuncName c f)
        
hsFuncName :: Class -> Function -> String 
hsFuncName c f = let (x:xs) = aliasedFuncName c f 
                 in (toLower x) : xs
                  
hsFuncXformer :: Function -> String 
hsFuncXformer func = let len = length (genericFuncArgs func) 
                     in "xform" ++ show len
                        
hsFuncXformerNew :: Function -> String 
hsFuncXformerNew func = let len = length (genericFuncArgs func) 
                        in if len > 0
                             then "xform" ++ show (len - 1)
                             else "xformnull" 


genericFuncRet :: Function -> Types 
genericFuncRet f = 
  case f of                        
    Constructor _ -> self_ 
    Virtual t _ _ -> t 
    NonVirtual t _ _ -> t
    AliasVirtual t _ _ _ -> t
    Destructor -> void_

genericFuncArgs :: Function -> Args 
genericFuncArgs Destructor = []
genericFuncArgs f = func_args f
                        
aliasedFuncName :: Class -> Function -> String 
aliasedFuncName c f = 
  case f of 
    Constructor _ -> constructorName c   
    Virtual _ str _ -> str 
    NonVirtual _ str _ -> nonvirtualName c str 
    AliasVirtual _ _  _ alias -> alias 
    Destructor -> destructorName  

cppFuncName :: Function -> String 
cppFuncName f =   case f of 
    Constructor _ -> "new"
    Virtual _ _  _ -> func_name f 
    NonVirtual _ _ _ -> func_name f  
    AliasVirtual _ _  _ _ -> func_name f 
    Destructor -> destructorName

constructorName :: Class -> String
constructorName c = "new" ++ (class_name c) 
 
nonvirtualName :: Class -> String -> String
nonvirtualName c str = firstLower (class_name c) ++ str 

destructorName :: String 
destructorName = "delete" 
