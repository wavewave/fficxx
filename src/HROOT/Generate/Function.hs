module HROOT.Generate.Function where

-- import Data.Char

import HROOT.Generate.CType
import HROOT.Generate.Util

data MethodType = Ordinary | Constructor | NonVirtual String | Alias String    
                deriving Eq 
                  

type Args = [(Types,String)]

data Function = Function { 
    func_ret  :: Types,
    func_name :: String,
    func_args :: Args,  
    func_type :: MethodType
  }

aliasedFuncName :: Function -> String 
aliasedFuncName func = let origname = func_name func 
                       in case func_type func of
                            Alias str -> str
                            NonVirtual str -> str
                            _ -> origname     

isNewFunc :: Function -> Bool 
isNewFunc func = case func_type func of
                   Constructor -> True 
                   _ -> False 
       
isVirtualFunc :: Function -> Bool 
isVirtualFunc func = case func_type func of 
                            NonVirtual _ -> False 
                            _ -> True 



argToString :: (Types,String) -> String 
argToString (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname 
argToString (SelfType, varname) = "Type ## _p " ++ varname
argToString (CPT (CPTClass cname) isconst, varname) = case isconst of 
  Const   -> "const_" ++ cname ++ "_p " ++ varname 
  NoConst -> cname ++ "_p " ++ varname
argToString _ = error "undefined argToString"

argsToString :: Args -> String 
argsToString args = 
  let args' = (SelfType, "p") : args 
  in  intercalateWith conncomma argToString args'

argsToStringNoSelf :: Args -> String 
argsToStringNoSelf = intercalateWith conncomma argToString 


argToCallString :: (Types,String) -> String
argToCallString (CPT (CPTClass str) _,varname) = 
  "to_nonconst<"++str++","++str++"_t>("++varname++")"
argToCallString (_,varname) = varname

argsToCallString :: Args -> String
argsToCallString = intercalateWith conncomma argToCallString

rettypeToString :: Types -> String 
rettypeToString (CT ctyp isconst) = ctypToStr ctyp isconst
rettypeToString Void = "void"
rettypeToString SelfType = "Type ## _p"
rettypeToString (CPT (CPTClass str) _) = str ++ "_p"

-- Function Declaration and Definition

funcToDecl :: Function -> String 
funcToDecl func 
  | func_name func /= "New" =  
    let tmpl = "$returntype$ Type ## _$funcname$ ( $args$ )" 
    in  render tmpl [ ("returntype", rettypeToString (func_ret func))  
                    , ("funcname", aliasedFuncName func) -- func_name func)
                    , ("args", argsToString (func_args func)) ] 
  | otherwise = 
    let tmpl = "$returntype$ Type ## _$funcname$ ( $args$ )" 
    in  render tmpl [ ("returntype", rettypeToString (func_ret func))  
                  , ("funcname",  aliasedFuncName func) -- func_name func)
                  , ("args", argsToStringNoSelf (func_args func)) ] 
  

funcsToDecls :: [Function] -> String 
funcsToDecls = intercalateWith connSemicolonBSlash funcToDecl


funcToDef :: Function -> String
funcToDef func 
  | func_name func /= "New" = 
    let declstr = funcToDecl func
        callstr = "to_nonconst<Type,Type ## _t>(p)->" 
                  ++ (func_name func) ++ "("
                  ++ argsToCallString (func_args func)   
                  ++ ")"
        returnstr = case (func_ret func) of          
          Void -> callstr ++ ";"
          SelfType -> "return to_nonconst<Type ## _t, Type>((Type *)" ++ callstr ++ ") ;"
          (CT _ctyp _isconst) -> "return "++callstr++";" 
          (CPT (CPTClass str) _) -> "return to_nonconst<"++str++"_t,"++str
                                    ++">(("++str++"*)"++callstr++");"
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | otherwise = 
    let declstr = funcToDecl func
        callstr = "(" ++ argsToCallString (func_args func) ++ ")"
        returnstr = "Type * newp = new Type " ++ callstr ++ "; \\\nreturn to_nonconst<Type ## _t, Type >(newp);"
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 

funcsToDefs :: [Function] -> String
funcsToDefs = intercalateWith connBSlash funcToDef


 
