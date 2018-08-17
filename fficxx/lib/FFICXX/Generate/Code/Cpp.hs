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

import           Data.Char
import           Data.Monoid                           ( (<>) )
--
import           FFICXX.Generate.Code.Primitive
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util

--
--
-- Class Declaration and Definition
--

----
---- Declaration
----

---- "Class Type Declaration" Instances

genCppHeaderTmplType :: Class -> String
genCppHeaderTmplType c = let tmpl = "// Opaque type definition for $classname \n\
                                    \typedef struct ${classname}_tag ${classname}_t; \n\
                                    \typedef ${classname}_t * ${classname}_p; \n\
                                    \typedef ${classname}_t const* const_${classname}_p; \n"
                      in subst tmpl (context [ ("classname", ffiClassName c) ])

genAllCppHeaderTmplType :: [Class] -> String
genAllCppHeaderTmplType = intercalateWith connRet2 (genCppHeaderTmplType)

---- "Class Declaration Virtual" Declaration

genCppHeaderTmplVirtual :: Class -> String
genCppHeaderTmplVirtual aclass =
  let tmpl = "#undef ${classname}_DECL_VIRT \n#define ${classname}_DECL_VIRT(Type) \\\n${funcdecl}"
      funcDeclStr = (funcsToDecls aclass) . virtualFuncs . class_funcs $ aclass
  in subst tmpl (context [ ("classname", map toUpper (ffiClassName aclass) )
                         , ("funcdecl" , funcDeclStr                     ) ])

genAllCppHeaderTmplVirtual :: [Class] -> String
genAllCppHeaderTmplVirtual = intercalateWith connRet2 genCppHeaderTmplVirtual

---- "Class Declaration Non-Virtual" Declaration

genCppHeaderTmplNonVirtual :: Class -> String
genCppHeaderTmplNonVirtual c =
  let tmpl = "#undef ${classname}_DECL_NONVIRT \n#define ${classname}_DECL_NONVIRT(Type) \\\n$funcdecl"
      declBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName c))
                                        , ("funcdecl" , funcDeclStr               ) ])
      funcDeclStr = (funcsToDecls c) . filter (not.isVirtualFunc)
                                     . class_funcs $ c
  in  declBodyStr

genAllCppHeaderTmplNonVirtual :: [Class] -> String
genAllCppHeaderTmplNonVirtual = intercalateWith connRet genCppHeaderTmplNonVirtual

---- "Class Declaration Virtual/NonVirtual" Instances

genCppHeaderInstVirtual :: (Class,Class) -> String
genCppHeaderInstVirtual (p,c) =
  let strc = map toUpper (ffiClassName p)
  in  strc<>"_DECL_VIRT(" <> ffiClassName c <> ");\n"

genCppHeaderInstNonVirtual :: Class -> String
genCppHeaderInstNonVirtual c =
  let strx = map toUpper (ffiClassName c)
  in  strx<>"_DECL_NONVIRT(" <> ffiClassName c <> ");\n"

genAllCppHeaderInstNonVirtual :: [Class] -> String
genAllCppHeaderInstNonVirtual =
  intercalateWith connRet genCppHeaderInstNonVirtual


----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefTmplVirtual :: Class -> String
genCppDefTmplVirtual aclass =
  let tmpl = "#undef ${classname}_DEF_VIRT\n#define ${classname}_DEF_VIRT(Type)\\\n$funcdef"
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName aclass) )
                                       , ("funcdef"  , funcDefStr                      ) ])
      funcDefStr = (funcsToDefs aclass) . virtualFuncs . class_funcs $ aclass
  in  defBodyStr

genAllCppDefTmplVirtual :: [Class] -> String
genAllCppDefTmplVirtual = intercalateWith connRet2 genCppDefTmplVirtual

---- "Class Definition NonVirtual" Declaration

genCppDefTmplNonVirtual :: Class -> String
genCppDefTmplNonVirtual aclass =
  let tmpl = "#undef ${classname}_DEF_NONVIRT\n#define ${classname}_DEF_NONVIRT(Type)\\\n$funcdef"
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (ffiClassName aclass) )
                                       , ("funcdef"  , funcDefStr                      ) ])
      funcDefStr = (funcsToDefs aclass) . filter (not.isVirtualFunc)
                                        . class_funcs $ aclass
  in  defBodyStr

genAllCppDefTmplNonVirtual :: [Class] -> String
genAllCppDefTmplNonVirtual = intercalateWith connRet2 genCppDefTmplNonVirtual

---- "Class Definition Virtual/NonVirtual" Instances

genCppDefInstVirtual :: (Class,Class) -> String
genCppDefInstVirtual (p,c) =
  let strc = map toUpper (ffiClassName p)
  in  strc<>"_DEF_VIRT(" <> ffiClassName c <> ")\n"

genCppDefInstNonVirtual :: Class -> String
genCppDefInstNonVirtual c =
  subst "${capitalclassname}_DEF_NONVIRT(${classname})"
    (context [ ("capitalclassname", toUppers (ffiClassName c))
             , ("classname"       , ffiClassName c           ) ])

genAllCppDefInstNonVirtual :: [Class] -> String
genAllCppDefInstNonVirtual = intercalateWith connRet genCppDefInstNonVirtual

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
  ctxt = context . (("suffix",if b then "_s" else ""):) $
                   case f of
                     TFunNew {..} -> [ ("tname"  , tclass_name       )
                                     , ("fname"  , "new"             )
                                     , ("decl"   , tmplFunToDecl b t f )
                                     , ("defn"   , tmplFunToDef b t f  ) ]
                     TFun {..}    -> [ ("tname"  , tclass_name       )
                                     , ("fname"  , tfun_name         )
                                     , ("decl"   , tmplFunToDecl b t f )
                                     , ("defn"   , tmplFunToDef b t f  ) ]
                     TFunDelete   -> [ ("tname"  , tclass_name       )
                                     , ("fname"  , "delete"          )
                                     , ("decl"   , tmplFunToDecl b t f )
                                     , ("defn"   , tmplFunToDef b t f  ) ]

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

  macro1 TFun {..}    = "  " <> tname<> "_" <> tfun_name <> suffix <> "(Type) \\"

  macro1 TFunNew {..} = "  " <> tname<> "_new(Type) \\"
  macro1 TFunDelete   = "  " <> tname<> "_delete(Type) \\"
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
             , ("args" , tmplAllArgsToString b Self t tfun_args )
             , ("ret"  , tmplRetTypeToString b tfun_ret    ) ])
tmplFunToDecl b t@TmplCls {..} TFunNew {..} =
  subst "$ret ${tname}_new_ ## Type ( $args )"
    (context [ ("tname", tclass_name                     )
             , ("args" , tmplAllArgsToString b NoSelf t tfun_new_args )
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
