module HROOT.Generate.Type.Method where

import HROOT.Generate.Type.CType
import HROOT.Generate.Util

type Args = [(Types,String)]

data Function = Constructor { func_args :: Args } 
              | Virtual { func_ret :: Types
                        , func_name :: String
                        , func_args :: Args } 
              | NonVirtual { func_ret :: Types 
                           , func_name :: String
                           , func_args :: Args }
              | Static     { func_ret :: Types 
                           , func_name :: String
                           , func_args :: Args }
              | AliasVirtual { func_ret :: Types 
                             , func_name :: String
                             , func_args :: Args 
                             , func_alias :: String }
              | Destructor  
              deriving Show


  
isNewFunc :: Function -> Bool 
isNewFunc (Constructor _ ) = True 
isNewFunc _ = False

isDeleteFunc :: Function -> Bool 
isDeleteFunc Destructor = True 
isDeleteFunc _ = False
       
isVirtualFunc :: Function -> Bool 
isVirtualFunc (Destructor)           = True
isVirtualFunc (Virtual _ _ _)        = True 
isVirtualFunc (AliasVirtual _ _ _ _) = True 
isVirtualFunc _ = False 

isStaticFunc :: Function -> Bool 
isStaticFunc (Static _ _ _) = True
isStaticFunc _ = False

virtualFuncs :: [Function] -> [Function] 
virtualFuncs = filter isVirtualFunc 

constructorFuncs :: [Function] -> [Function]
constructorFuncs = filter isNewFunc

nonVirtualNotNewFuncs :: [Function] -> [Function]
nonVirtualNotNewFuncs = 
  filter (\x -> (not.isVirtualFunc) x && (not.isNewFunc) x && (not.isDeleteFunc) x && (not.isStaticFunc) x )

staticFuncs :: [Function] -> [Function] 
staticFuncs = filter isStaticFunc

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

