module Function where

-- import Data.Char

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers


import CType
import Util
import Templates


type Args = [(Types,String)]

data Function = Function { 
    func_ret  :: Types,
    func_name :: String,
    func_args :: Args 
  }

argToString :: (Types,String) -> String 
argToString (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname 
argToString (SelfType, varname) = "Type ## _p " ++ varname


argsToString :: Args -> String 
argsToString args = 
  let args' = (SelfType, "p") : args 
  in  intercalateWith conncomma argToString args'

argsToStringNoSelf :: Args -> String 
argsToStringNoSelf = intercalateWith conncomma argToString 

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
funcToDecl func | func_name func /= "New" =  
  let tmpl = "$returntype$ Type ## _$funcname$ ( $args$ )" 
  in  render tmpl [ ("returntype", rettypeToString (func_ret func))  
                  , ("funcname", func_name func)
                  , ("args", argsToString (func_args func)) ] 
                | func_name func == "New" = 
  let tmpl = "$returntype$ Type ## _$funcname$ ( $args$ )" 
  in  render tmpl [ ("returntype", rettypeToString (func_ret func))  
                  , ("funcname", func_name func)
                  , ("args", argsToStringNoSelf (func_args func)) ] 
  

funcsToDecls :: [Function] -> String 
funcsToDecls = intercalateWith connSemicolonBSlash funcToDecl


funcToDef :: Function -> String
funcToDef func | func_name func /= "New" = 
  let declstr = funcToDecl func
      callstr = "to_nonconst<Type,Type ## _t>(p)->" 
                ++ (func_name func) ++ "("
                ++ argsToCallString (func_args func)   
                ++ ")"
      returnstr = case (func_ret func) of          
        Void -> callstr ++ ";"
        SelfType -> "return to_nonconst<Type ## _, Type>((Type *)" ++ callstr ++ ") ;"
        (CT ctyp isconst) -> "return "++callstr++";" 
        (CPT (CPTClass str) _) -> "return to_nonconst<"++str++"_t,"++str
                                  ++">(("++str++"*)"++callstr++");"
  in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
funcToDef func | func_name func == "New" = 
  let declstr = funcToDecl func
      callstr = "(" ++ argsToCallString (func_args func) ++ ")"
      returnstr = "Type * newp = new Type " ++ callstr ++ "; \\\nreturn to_nonconst<Type ## _t, Type >(newp);"
  in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 



funcsToDefs :: [Function] -> String
funcsToDefs = intercalateWith connBSlash funcToDef


 
