module Function where

-- import Data.Char

import Text.StringTemplate
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

argsToString :: Args -> String 
argsToString = intercalateWith conncomma argToString 

argToCallString (_,varname) = varname

argsToCallString :: Args -> String
argsToCallString = intercalateWith conncomma argToCallString

rettypeToString :: Types -> String 
rettypeToString (CT ctyp isconst) = ctypToStr ctyp isconst
rettypeToString Void = "void"
rettypeToString SelfType = "Type ## _p"
rettypeToString (CPT (CPTClass str) _) = str ++ "_p"

-- Function Declaration and Definition

funcToDecl :: STGroup String -> Function -> String 
funcToDecl templates func =  
  renderTemplateGroup templates 
                      [ ("returntype", rettypeToString (func_ret func))  
                      , ("funcname", func_name func)
                      , ("args", argsToString (func_args func)) ] 
                      funcdeclTemplate

funcsToDecls :: STGroup String -> [Function] -> String 
funcsToDecls templates = intercalateWith connSemicolonBSlash (funcToDecl templates)


funcToDef :: STGroup String -> Function -> String
funcToDef templates func = 
  let declstr = funcToDecl templates func
      callstr = "to_nonconst<Type,Type ## _t>(p)->" 
                ++ (func_name func) 
                ++ argsToCallString (func_args func)   
      returnstr = case (func_ret func) of          
        Void -> callstr ++ ";"
        SelfType -> "return to_nonconst<Type ## _, Type>((Type *)" ++ callstr ++ ") ;"
        (CT ctyp isconst) -> "return "++callstr++";" 
        (CPT (CPTClass str) _) -> "return to_nonconst<"++str++"_t,"++str
                                  ++">(("++str++"*)"++callstr++");"
  in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 

funcsToDefs :: STGroup String -> [Function] -> String
funcsToDefs templates = intercalateWith connBSlash (funcToDef templates)


 
