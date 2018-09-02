{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Cpp
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.Cpp where

import Data.Char
import Data.List                             (intercalate)
import Data.Monoid                           ((<>))
--
import FFICXX.Generate.Code.Primitive        (accessorCFunSig
                                             ,argsToCallString
                                             ,argsToString
                                             ,argsToStringNoSelf
                                             ,castCpp2C
                                             ,castC2Cpp
                                             ,CFunSig(..)
                                             ,genericFuncArgs
                                             ,genericFuncRet
                                             ,rettypeToString
                                             ,tmplMemFuncArgToString
                                             ,tmplMemFuncRetTypeToString
                                             ,tmplAllArgsToCallString
                                             ,tmplAllArgsToString
                                             ,tmplRetTypeToString)
import FFICXX.Generate.Name                  (aliasedFuncName
                                             ,cppFuncName
                                             ,ffiClassName
                                             ,ffiTmplFuncName
                                             ,hsTemplateMemberFunctionName)
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface
import FFICXX.Generate.Util

--
--
-- Class Declaration and Definition
--

----
---- Declaration
----

---- "Class Type Declaration" Instances

genCppHeaderMacroType :: Class -> String
genCppHeaderMacroType c = let tmpl = "// Opaque type definition for $classname \n\
                                    \typedef struct ${classname}_tag ${classname}_t; \n\
                                    \typedef ${classname}_t * ${classname}_p; \n\
                                    \typedef ${classname}_t const* const_${classname}_p; \n"
                      in subst tmpl (context [ ("classname", ffiClassName c) ])


---- "Class Declaration Virtual" Declaration

genCppHeaderMacroVirtual :: Class -> String
genCppHeaderMacroVirtual aclass =
  let tmpl = "#undef ${classname}_DECL_VIRT \n#define ${classname}_DECL_VIRT(Type) \\\n${funcdecl}"
      funcDeclStr = (funcsToDecls aclass) . virtualFuncs . class_funcs $ aclass
  in subst tmpl (context [ ("classname", map toUpper (ffiClassName aclass) )
                         , ("funcdecl" , funcDeclStr                     ) ])


---- "Class Declaration Non-Virtual" Declaration

genCppHeaderMacroNonVirtual :: Class -> String
genCppHeaderMacroNonVirtual c =
  let tmpl = "#undef ${classname}_DECL_NONVIRT \n#define ${classname}_DECL_NONVIRT(Type) \\\n$funcdecl"
      declBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName c))
                                        , ("funcdecl" , funcDeclStr               ) ])
      funcDeclStr = (funcsToDecls c) . filter (not.isVirtualFunc)
                                     . class_funcs $ c
  in  declBodyStr


---- "Class Declaration Accessor" Declaration

genCppHeaderMacroAccessor :: Class -> String
genCppHeaderMacroAccessor c =
  let tmpl = "#undef ${classname}_DECL_ACCESSOR\n#define ${classname}_DECL_ACCESSOR(Type)\\\n$funcdecl"
      declBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName c))
                                        , ("funcdecl" , funcDeclStr               ) ])
      funcDeclStr = accessorsToDecls (class_vars c)
  in  declBodyStr


---- "Class Declaration Virtual/NonVirtual/Accessor" Instances

genCppHeaderInstVirtual :: (Class,Class) -> String
genCppHeaderInstVirtual (p,c) =
  let strc = map toUpper (ffiClassName p)
  in  strc<>"_DECL_VIRT(" <> ffiClassName c <> ");\n"

genCppHeaderInstNonVirtual :: Class -> String
genCppHeaderInstNonVirtual c =
  let strx = map toUpper (ffiClassName c)
  in  strx<>"_DECL_NONVIRT(" <> ffiClassName c <> ");\n"


genCppHeaderInstAccessor :: Class -> String
genCppHeaderInstAccessor c =
  let strx = map toUpper (ffiClassName c)
  in  strx<>"_DECL_ACCESSOR(" <> ffiClassName c <> ");\n"


----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefMacroVirtual :: Class -> String
genCppDefMacroVirtual aclass =
  let tmpl = "#undef ${classname}_DEF_VIRT\n#define ${classname}_DEF_VIRT(Type)\\\n$funcdef"
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName aclass) )
                                       , ("funcdef"  , funcDefStr                      ) ])
      funcDefStr = (funcsToDefs aclass) . virtualFuncs . class_funcs $ aclass
  in  defBodyStr


---- "Class Definition NonVirtual" Declaration

genCppDefMacroNonVirtual :: Class -> String
genCppDefMacroNonVirtual aclass =
  let tmpl = "#undef ${classname}_DEF_NONVIRT\n#define ${classname}_DEF_NONVIRT(Type)\\\n$funcdef"
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName aclass) )
                                       , ("funcdef"  , funcDefStr                      ) ])
      funcDefStr = (funcsToDefs aclass) . filter (not.isVirtualFunc)
                                        . class_funcs $ aclass
  in  defBodyStr


---- Define Macro to provide Accessor C-C++ shim code for a class

genCppDefMacroAccessor :: Class -> String
genCppDefMacroAccessor c =
  let tmpl = "#undef ${classname}_DEF_ACCESSOR\n#define ${classname}_DEF_ACCESSOR(Type)\\\n$funcdef"
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName c))
                                       , ("funcdef"  , funcDefStr                  ) ])
      funcDefStr = accessorsToDefs (class_vars c)
  in  defBodyStr

---- Define Macro to provide TemplateMemberFunction C-C++ shim code for a class

genCppDefMacroTemplateMemberFunction :: Class -> TemplateMemberFunction -> String
genCppDefMacroTemplateMemberFunction c f = subst tmpl ctxt
  where
    tmpl = "#define ${macroname}(Type) \\\n\
           \  extern \"C\" { \\\n\
           \    $decl; \\\n\
           \  } \\\n\
           \  inline $defn \\\n\
           \  auto a_${macroname}_##Type = ${macroname}_##Type  ;\n"
    ctxt = context
             [ ("macroname", hsTemplateMemberFunctionName c f)
             , ("decl"     , tmplMemberFunToDecl c f)
             , ("defn"     , tmplMemberFunToDef c f)
             ]


---- Invoke Macro to define Virtual/NonVirtual method for a class

genCppDefInstVirtual :: (Class,Class) -> String
genCppDefInstVirtual (p,c) =
  let strc = map toUpper (ffiClassName p)
  in  strc<>"_DEF_VIRT(" <> ffiClassName c <> ")\n"

genCppDefInstNonVirtual :: Class -> String
genCppDefInstNonVirtual c =
  subst "${capitalclassname}_DEF_NONVIRT(${classname})"
    (context [ ("capitalclassname", toUppers (ffiClassName c))
             , ("classname"       , ffiClassName c           ) ])

genCppDefInstAccessor :: Class -> String
genCppDefInstAccessor c =
  subst "${capitalclassname}_DEF_ACCESSOR(${classname})"
    (context [ ("capitalclassname", toUppers (ffiClassName c))
             , ("classname"       , ffiClassName c           ) ])

-----------------

genAllCppHeaderInclude :: ClassImportHeader -> String
genAllCppHeaderInclude header =
    intercalateWith connRet (\x->"#include \""<>x<>"\"") $
      map unHdrName (cihIncludedHPkgHeadersInCPP header
                     <> cihIncludedCPkgHeaders header)

----



-------------------------
-- TOP LEVEL FUNCTIONS --
-------------------------

genTopLevelFuncCppHeader :: TopLevelFunction -> String
genTopLevelFuncCppHeader TopLevelFunction {..} =
  subst "$returntype $funcname ( $args );"
    (context [ ("returntype", rettypeToString toplevelfunc_ret                )
             , ("funcname"  , "TopLevel_"
                              <> maybe toplevelfunc_name id toplevelfunc_alias)
             , ("args"      , argsToStringNoSelf toplevelfunc_args            ) ])
genTopLevelFuncCppHeader TopLevelVariable {..} =
  subst "$returntype $funcname ( );"
    (context [ ("returntype", rettypeToString toplevelvar_ret                )
             , ("funcname"  , "TopLevel_"
                               <> maybe toplevelvar_name id toplevelvar_alias) ])

genTopLevelFuncCppDefinition :: TopLevelFunction -> String
genTopLevelFuncCppDefinition TopLevelFunction {..} =
  let tmpl = "$returntype $funcname ( $args ) { \n  $funcbody\n}"
      callstr = toplevelfunc_name <> "("
                <> argsToCallString toplevelfunc_args
                <> ")"
      funcDefStr = returnCpp False (toplevelfunc_ret) callstr
  in subst tmpl (context [ ("returntype", rettypeToString toplevelfunc_ret                )
                         , ("funcname"  , "TopLevel_"
                                          <> maybe toplevelfunc_name id toplevelfunc_alias)
                         , ("args"      , argsToStringNoSelf toplevelfunc_args            )
                         , ("funcbody"  , funcDefStr                                      ) ])
genTopLevelFuncCppDefinition TopLevelVariable {..} =
  let tmpl = "$returntype $funcname ( ) { \n  $funcbody\n}"
      callstr = toplevelvar_name
      funcDefStr = returnCpp False (toplevelvar_ret) callstr
  in subst tmpl (context [ ("returntype", rettypeToString toplevelvar_ret               )
                         , ("funcname"  , "TopLevel_"
                                          <> maybe toplevelvar_name id toplevelvar_alias)
                         , ("funcbody"  , funcDefStr                                    ) ])


genTmplFunCpp :: Bool -- ^ is for simple type?
              -> TemplateClass
              -> TemplateFunction
              -> String
genTmplFunCpp b t@TmplCls {..} f = subst tmpl ctxt
 where
  tmpl = "#define ${tname}_${fname}${suffix}(Type) \\\n\
         \  extern \"C\" { \\\n\
         \    $decl; \\\n\
         \  } \\\n\
         \  inline $defn \\\n\
         \  auto a_${tname}_${fname}_ ## Type = ${tname}_${fname}_ ## Type  ;\n"
  ctxt = context $
           (("suffix",if b then "_s" else ""):) $
             [ ("tname"  , tclass_name )
             , ("fname"  , ffiTmplFuncName f)
             , ("decl"   , tmplFunToDecl b t f )
             , ("defn"   , tmplFunToDef b t f ) ]

genTmplClassCpp :: Bool -- ^ is for simple type
                -> TemplateClass
                -> [TemplateFunction]
                -> String
genTmplClassCpp b TmplCls {..} fs = subst tmpl ctxt
 where
  tmpl = "#define ${tname}_instance${suffix}(Type) \\\n\
         \$macro\n"
  suffix = if b then "_s" else ""
  ctxt = context [ ("tname"  , tclass_name )
                 , ("suffix" , suffix      )
                 , ("macro"  , macro       ) ]
  tname = tclass_name
  macro1 f@TFun {..}    = "  " <> tname<> "_" <> ffiTmplFuncName f <> suffix <> "(Type) \\"

  macro1 f@TFunNew {..} = "  " <> tname<> "_" <> ffiTmplFuncName f <> "(Type) \\"
  macro1 TFunDelete     = "  " <> tname<> "_delete(Type) \\"
  macro = intercalateWith connRet macro1 fs

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
                               where str = ffiClassName c'
    CPT (CPTClassRef c') _  -> "return to_nonconst<"<>str<>"_t,"<>str
                               <>">(&("<>callstr<>"));"
                               where str = ffiClassName c'
    CPT (CPTClassCopy c') _ -> "return to_nonconst<"<>str<>"_t,"<>str
                               <>">(new "<>str<>"("<>callstr<>"));"
                               where str = ffiClassName c'
    CPT (CPTClassMove c') _ -> -- TODO: check whether this is working or not.
                               "return std::move(to_nonconst<"<>str<>"_t,"<>str
                               <>">(&("<>callstr<>")));"
                               where str = ffiClassName c'
    TemplateApp (TemplateAppInfo _ _ cpptype) ->
         cpptype <> "* r = new " <> cpptype <> "(" <> callstr <> "); "
      <> "return (static_cast<void*>(r));"
    TemplateAppRef (TemplateAppInfo _ _ cpptype) ->
         cpptype <> "* r = new " <> cpptype <> "(" <> callstr <> "); "
      <> "return (static_cast<void*>(r));"
    TemplateAppMove (TemplateAppInfo _ _ cpptype) ->
         cpptype <> "* r = new " <> cpptype <> "(" <> callstr <> "); "
      <> "return std::move(static_cast<void*>(r));"
    TemplateType _          -> error "returnCpp: TemplateType"
    TemplateParam _         ->
      if b then "return (" <> callstr <> ");"
           else "return to_nonconst<Type ## _t, Type>((Type *)&("
                <> callstr <> ")) ;"
    TemplateParamPointer _  ->
      if b then "return (" <> callstr <> ");"
           else "return to_nonconst<Type ## _t, Type>("
                <> callstr <> ") ;"



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
tmplFunToDecl b t@TmplCls {..} f@TFun {..} =
  subst "$ret ${tname}_${fname}_ ## Type ( $args )"
    (context [ ("tname", tclass_name)
             , ("fname", ffiTmplFuncName f)
             , ("args" , tmplAllArgsToString b Self t tfun_args)
             , ("ret"  , tmplRetTypeToString b tfun_ret) ])
tmplFunToDecl b t@TmplCls {..} f@TFunNew {..} =
  subst "$ret ${tname}_${fname}_ ## Type ( $args )"
    (context [ ("tname", tclass_name)
             , ("fname", ffiTmplFuncName f)
             , ("args" , tmplAllArgsToString b NoSelf t tfun_new_args)
             , ("ret"  , tmplRetTypeToString b (TemplateType t)) ])
tmplFunToDecl b t@TmplCls {..} TFunDelete =
  subst "$ret ${tname}_delete_ ## Type ( $args )"
    (context [ ("tname", tclass_name                     )
             , ("args" , tmplAllArgsToString b Self t [] )
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
      TFun {..}    -> "(static_cast<" <> tclass_oname <> "<Type>*>(p))->"
                      <> tfun_oname <> "("
                      <> tmplAllArgsToCallString b tfun_args
                      <> ")"
      TFunNew {..} -> "new " <> tclass_oname <> "<Type>("
                      <> tmplAllArgsToCallString b tfun_new_args
                      <> ")"
      TFunDelete   -> "delete (static_cast<" <> tclass_oname <> "<Type>*>(p))"
  returnstr =
    case f of
      TFunNew {..} -> "return static_cast<void*>("<>callstr<>");"
      TFunDelete   -> callstr <> ";"
      TFun {..} -> returnCpp b (tfun_ret) callstr


-- Accessor Declaration and Definition

accessorToDecl :: Variable -> Accessor -> String
accessorToDecl v a =
  let tmpl = "$returntype Type ## _$funcname ( $args )"
      csig = accessorCFunSig (var_type v) a
  in subst tmpl (context [ ("returntype", rettypeToString (cRetType csig))
                         , ("funcname"  , var_name v <> "_" <> case a of Getter -> "get"; Setter -> "set")
                         , ("args"      , argsToString (cArgTypes csig))
                         ])

accessorsToDecls :: [Variable] -> String
accessorsToDecls vs =
  let dcls = concatMap (\v -> [accessorToDecl v Getter,accessorToDecl v Setter]) vs
  in intercalate "; \\\n" dcls


accessorToDef :: Variable -> Accessor -> String
accessorToDef v a =
  let declstr = accessorToDecl v a
      varexp = "to_nonconst<Type,Type ## _t>(p)->" <> var_name v
      body Getter = "return (" <> castCpp2C (var_type v) varexp <> ");"
      body Setter =    varexp
                    <> " = "
                    <> castC2Cpp (var_type v) "x"  -- TODO: somehow clean up this hard-coded "x".
                    <> ";"
  in  intercalate "\\\n" [declstr, "{", body a, "}"]


accessorsToDefs :: [Variable] -> String
accessorsToDefs vs =
  let defs = concatMap (\v -> [accessorToDef v Getter,accessorToDef v Setter]) vs
  in intercalate "; \\\n" defs



-- Template Member Function Declaration and Definition

-- TODO: Handle simple type
tmplMemberFunToDecl :: Class -> TemplateMemberFunction -> String
tmplMemberFunToDecl c f =
  subst "$ret ${macroname}_##Type ( $args )"
    (context [ ("macroname", hsTemplateMemberFunctionName c f)
             , ("args"     , intercalateWith conncomma (tmplMemFuncArgToString c) ((SelfType,"p"):tmf_args f))
             , ("ret"      , tmplMemFuncRetTypeToString c (tmf_ret f))
             ])


-- TODO: Handle simple type
tmplMemberFunToDef :: Class -> TemplateMemberFunction -> String
tmplMemberFunToDef c f =
    intercalateWith connBSlash id [ declstr
                                  , "  {"
                                  , "    " <> returnstr
                                  , "  }"
                                  ]
  where
    declstr = tmplMemberFunToDecl c f
    callstr =    "(to_nonconst<" <> ffiClassName c  <> "," <> ffiClassName c <> "_t" <> ">(p))"
              <> "->"
              <> tmf_name f
              <> "<Type>"
              <> "(" <> tmplAllArgsToCallString False (tmf_args f) <> ")"
    returnstr = returnCpp False (tmf_ret f) callstr
