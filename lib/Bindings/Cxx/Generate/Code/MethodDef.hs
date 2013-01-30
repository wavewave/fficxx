module Bindings.Cxx.Generate.Code.MethodDef where

-- import Bindings.Cxx.Generate.Type.CType
-- import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Util 


-- Function Declaration and Definition

funcToDecl :: Class -> Function -> String 
funcToDecl c func 
  | isNewFunc func || isStaticFunc func = 
    let tmpl = "$returntype$ Type ## _$funcname$ ( $args$ )" 
    in  render tmpl [ ("returntype", rettypeToString (genericFuncRet func))  
                    , ("funcname",  aliasedFuncName c func) 
                    , ("args", argsToStringNoSelf (genericFuncArgs func)) ] 
  | otherwise =  
    let tmpl = "$returntype$ Type ## _$funcname$ ( $args$ )" 
    in  render tmpl [ ("returntype", rettypeToString (genericFuncRet func))  
                    , ("funcname", aliasedFuncName c func) 
                    , ("args", argsToString (genericFuncArgs func)) ] 



funcsToDecls :: Class -> [Function] -> String 
funcsToDecls c = intercalateWith connSemicolonBSlash (funcToDecl c)


funcToDef :: Class -> Function -> String
funcToDef c func 
  | isNewFunc func = 
    let declstr = funcToDecl c func
        callstr = "(" ++ argsToCallString (genericFuncArgs func) ++ ")"
        returnstr = "Type * newp = new Type " ++ callstr ++ "; \\\nreturn to_nonconst<Type ## _t, Type >(newp);"
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | isDeleteFunc func = 
    let declstr = funcToDecl c func
     --   callstr = "( Type ## _p p )"
        returnstr = "delete (to_nonconst<Type,Type ## _t>(p)) ; "
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | isStaticFunc func = 
    let declstr = funcToDecl c func
        callstr = cppFuncName c func ++ "("
                  ++ argsToCallString (genericFuncArgs func)   
                  ++ ")"
        returnstr = case (genericFuncRet func) of          
          Void -> callstr ++ ";"
          SelfType -> "return to_nonconst<Type ## _t, Type>((Type *)" ++ callstr ++ ") ;"
          (CT _ctyp _isconst) -> "return "++callstr++";" 
          (CPT (CPTClass c') _) -> "return to_nonconst<"++str++"_t,"++str
                                    ++">(("++str++"*)"++callstr++");"
            where str = class_name c' 
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | otherwise = 
    let declstr = funcToDecl c func
        callstr = -- "to_nonconst<Type,Type ## _t>(p)->" 
                  "TYPECASTMETHOD(Type,"++ aliasedFuncName c func ++ "," ++ class_name c ++ ")(p)->"
                  ++ cppFuncName c func ++ "("
                  ++ argsToCallString (genericFuncArgs func)   
                  ++ ")"
        returnstr = case (genericFuncRet func) of          
          Void -> callstr ++ ";"
          SelfType -> "return to_nonconst<Type ## _t, Type>((Type *)" ++ callstr ++ ") ;"
          (CT _ctyp _isconst) -> "return "++callstr++";" 
          (CPT (CPTClass c') _) -> "return to_nonconst<"++str++"_t,"++str
                                    ++">(("++str++"*)"++callstr++");"
             where str = class_name c'
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 



funcsToDefs :: Class -> [Function] -> String
funcsToDefs c = intercalateWith connBSlash (funcToDef c)






