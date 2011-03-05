module Function where

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

data Class = Class { 
    class_name :: String, 
    class_funcs :: [Function]
  }


argToString :: (Types,String) -> String 
argToString (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname 

argsToString :: Args -> String 
argsToString = intercalateWith conncomma argToString 
--foldl1 (\x y -> x `conncomma` y) . (map argToString)  

rettypeToString :: Types -> String 
rettypeToString (CT ctyp isconst) = ctypToStr ctyp isconst
rettypeToString Void = "void"
rettypeToString Type = "Type ## _p"
rettypeToString (CPT (CPTClass str) _) = str ++ "_p"

funcToDecl :: STGroup String -> Function -> String 
funcToDecl templates func =  
  renderTemplateGroup templates 
                      [ ("returntype", rettypeToString (func_ret func))  
                      , ("funcname", func_name func)
                      , ("args", argsToString (func_args func)) ] 
                      funcdeclTemplate

funcsToDecls :: STGroup String -> [Function] -> String 
funcsToDecls templates = intercalateWith connSemicolonBSlash (funcToDecl templates)
--  foldl1 (\x y -> x `connSemicolonBSlash` y) . (map (funcToDecl templates)) 




classToDecl :: STGroup String -> Class -> String 
classToDecl templates aclass =  
  let declBodyStr = renderTemplateGroup templates 
                                        [ ("classname", class_name aclass ) 
                                        , ("funcdecl" , funcDeclStr ) ] 
                                        declbodyTemplate 
      funcDeclStr = funcsToDecls templates (class_funcs aclass)
        
  in  declBodyStr 
      

classesToDecls :: STGroup String -> [Class] -> String 
classesToDecls templates = intercalateWith connRet (classToDecl templates)

mkDeclHeader :: STGroup String -> [Class] -> String 
mkDeclHeader templates classes = 
  let decl        = renderTemplateGroup templates 
                                        [ ("declarationbody", declBodyStr ) ] 
                                        declarationTemplate
      declBodyStr = classesToDecls templates classes
  in  decl