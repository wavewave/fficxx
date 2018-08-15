{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.MethodDef
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.MethodDef where

import           Data.Monoid                ( (<>) )
--
import           FFICXX.Generate.Code.Primitive
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Util 


returnCpp :: Bool  -- ^ for simple type
          -> Types
          -> String -- ^ call string
          -> String
returnCpp b ret callstr = 
  case ret of          
    Void                    -> callstr <> ";"
    SelfType                -> "return to_nonconst<Type ## _t, Type>((Type *)"
                                <> callstr <> ") ;"
    CT (CRef _) _           -> "return (&("<>callstr<>"));"
    CT _ _                  -> "return "<>callstr<>";" 
    CPT (CPTClass c') _     -> "return to_nonconst<"<>str<>"_t,"<>str
                               <>">(("<>str<>"*)"<>callstr<>");"
                               where str = class_name c'
    CPT (CPTClassRef c') _  -> "return to_nonconst<"<>str<>"_t,"<>str
                               <>">(&("<>callstr<>"));"
                               where str = class_name c'
    CPT (CPTClassCopy c') _ -> "return to_nonconst<"<>str<>"_t,"<>str
                               <>">(new "<>str<>"("<>callstr<>"));"
                               where str = class_name c'

    TemplateApp _ _ _       -> "return (" <> callstr <> ");"
    TemplateAppRef _ _ _    -> "return (&(" <> callstr <> "));"  
    TemplateType _          -> error "returnCpp: TemplateType"
    TemplateParam _         ->
      if b then "return (" <> callstr <> ");"
           else "return to_nonconst<Type ## _t, Type>((Type *)&("
                <> callstr <> ")) ;"



-- Function Declaration and Definition

funcToDecl :: Class -> Function -> String 
funcToDecl c func 
  | isNewFunc func || isStaticFunc func = 
    let tmpl = "$returntype Type ## _$funcname ( $args )" 
    in subst tmpl (context [ ("returntype", rettypeToString (genericFuncRet func))  
                           , ("funcname",  aliasedFuncName c func) 
                           , ("args", argsToStringNoSelf (genericFuncArgs func))
                           ])
  | otherwise =  
    let tmpl = "$returntype Type ## _$funcname ( $args )" 
    in subst tmpl (context [ ("returntype", rettypeToString (genericFuncRet func))  
                           , ("funcname", aliasedFuncName c func) 
                           , ("args", argsToString (genericFuncArgs func))
                           ]) 



funcsToDecls :: Class -> [Function] -> String 
funcsToDecls c = intercalateWith connSemicolonBSlash (funcToDecl c)


funcToDef :: Class -> Function -> String
funcToDef c func 
  | isNewFunc func = 
    let declstr = funcToDecl c func
        callstr = "(" <> argsToCallString (genericFuncArgs func) <> ")"
        returnstr = "Type * newp = new Type " <> callstr <> "; \\\nreturn to_nonconst<Type ## _t, Type >(newp);"
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | isDeleteFunc func = 
    let declstr = funcToDecl c func
        returnstr = "delete (to_nonconst<Type,Type ## _t>(p)) ; "
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | isStaticFunc func = 
    let declstr = funcToDecl c func
        callstr = cppFuncName c func <> "("
                  <> argsToCallString (genericFuncArgs func)   
                  <> ")"
        returnstr = returnCpp False (genericFuncRet func) callstr
    in intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 
  | otherwise = 
    let declstr = funcToDecl c func
        callstr = "to_nonconst<Type,Type ## _t>(p)->"
                  <> cppFuncName c func <> "("
                  <> argsToCallString (genericFuncArgs func)   
                  <> ")"
        returnstr = returnCpp False (genericFuncRet func) callstr
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"] 



funcsToDefs :: Class -> [Function] -> String
funcsToDefs c = intercalateWith connBSlash (funcToDef c)


tmplFunToDecl :: Bool -> TemplateClass -> TemplateFunction -> String 
tmplFunToDecl b t@TmplCls {..} TFun {..} =  
  subst "$ret ${tname}_${fname}_ ## Type ( $args )"
    (context [ ("tname", tclass_name                     )  
             , ("fname", tfun_name                       ) 
             , ("args" , tmplAllArgsToString Self t tfun_args )
             , ("ret"  , tmplRetTypeToString b tfun_ret    ) ]) 
tmplFunToDecl b t@TmplCls {..} TFunNew {..} =  
  subst "$ret ${tname}_new_ ## Type ( $args )"
    (context [ ("tname", tclass_name                     )  
             , ("args" , tmplAllArgsToString NoSelf t tfun_new_args )
             , ("ret"  , tmplRetTypeToString b (TemplateType t)) ]) 
tmplFunToDecl _ t@TmplCls {..} TFunDelete =  
  subst "$ret ${tname}_delete_ ## Type ( $args )"
    (context [ ("tname", tclass_name                     )  
             , ("args" , tmplAllArgsToString Self t [] )
             , ("ret"  , "void" ) ]) 



tmplFunToDef :: Bool -- ^ for simple type
             -> TemplateClass
             -> TemplateFunction
             -> String
tmplFunToDef b t@TmplCls {..} f = intercalateWith connBSlash id [declstr, "  {", "    "<>returnstr, "  }"]
 where
  declstr = tmplFunToDecl b t f
  callstr =
    case f of
      TFun {..}    -> "(reinterpret_cast<" <> tclass_oname <> "<Type>*>(p))->"
                      <> tfun_oname <> "("
                      <> tmplAllArgsToCallString tfun_args   
                      <> ")"
      TFunNew {..} -> "new " <> tclass_oname <> "<Type>("
                      <> tmplAllArgsToCallString tfun_new_args
                      <> ")"
      TFunDelete   -> "delete (reinterpret_cast<" <> tclass_oname <> "<Type>*>(p))"
                      
  returnstr =
    case f of
      TFunNew {..} -> "return reinterpret_cast<void*>("<>callstr<>");"
      TFunDelete   -> callstr <> ";"
      TFun {..} -> returnCpp b (tfun_ret) callstr




