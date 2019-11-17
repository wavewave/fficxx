{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.Cpp where

import Data.Char
import Data.List                             (intercalate)
import Data.Monoid                           ((<>))
--
import qualified FFICXX.Runtime.CodeGen.C as R
--
import FFICXX.Generate.Code.Primitive        ( accessorCFunSig
                                             , argsToCallString
                                             , argsToCTypVar
                                             , argsToCTypVarNoSelf
                                             , castCpp2C
                                             , castC2Cpp
                                             , CFunSig(..)
                                             , genericFuncArgs
                                             , genericFuncRet
                                             , rettypeToString
                                             , tmplMemFuncArgToCTypVar
                                             , tmplMemFuncRetTypeToString
                                             , tmplAllArgsToCallString
                                             , tmplAllArgsToString
                                             , tmplRetTypeToString
                                             )
import FFICXX.Generate.Name                  ( aliasedFuncName
                                             , cppFuncName
                                             , ffiClassName
                                             , ffiTmplFuncName
                                             , hsTemplateMemberFunctionName
                                             )
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Util

--
--
-- Class Declaration and Definition
--

----
---- Declaration
----

---- "Class Type Declaration" Instances

genCppHeaderMacroType :: Class -> [R.CStatement]
genCppHeaderMacroType c =
    [ R.Comment "Opaque type definition for $classname"
    , R.TypeDef (R.CType ("struct " <> classname_tag)) (R.sname classname_t)
    , R.TypeDef (R.CType (classname_t <> " *"))        (R.sname classname_p)
    , R.TypeDef (R.CType (classname_t <> " const*"))   (R.sname ("const_" <> classname_p))
    ]
  where
    classname = ffiClassName c
    classname_tag = classname <> "_tag"
    classname_t   = classname <> "_t"
    classname_p   = classname <> "_p"

---- "Class Declaration Virtual" Declaration

genCppHeaderMacroVirtual :: Class -> R.CMacro
genCppHeaderMacroVirtual aclass =
  let funcDecls = map R.CDeclaration
                . funcsToDecls aclass
                . virtualFuncs
                . class_funcs
                $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DECL_VIRT"
  in R.Define (R.sname macroname) (R.sname "Type") funcDecls

genCppHeaderMacroNonVirtual :: Class -> R.CMacro
genCppHeaderMacroNonVirtual c =
  let funcDecls = map R.CDeclaration
                . funcsToDecls c
                . filter (not.isVirtualFunc)
                . class_funcs
                $ c
      macrocname = map toUpper (ffiClassName c)
      macroname = macrocname <> "_DECL_NONVIRT"
  in R.Define (R.sname macroname) (R.sname "Type") funcDecls


---- "Class Declaration Accessor" Declaration

genCppHeaderMacroAccessor :: Class -> R.CMacro
genCppHeaderMacroAccessor c =
  let funcDecls  = map R.CDeclaration $ accessorsToDecls (class_vars c)
      macrocname = map toUpper (ffiClassName c)
      macroname  = macrocname <> "_DECL_ACCESSOR"
  in R.Define (R.sname macroname) (R.sname "Type") funcDecls


---- "Class Declaration Virtual/NonVirtual/Accessor" Instances

genCppHeaderInstVirtual :: (Class,Class) -> R.CStatement
genCppHeaderInstVirtual (p,c) =
  let macroname = map toUpper (ffiClassName p) <> "_DECL_VIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

     -- strc<>"_DECL_VIRT(" <> ffiClassName c <> ");\n"

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

genCppDefMacroVirtual :: Class -> R.CMacro
genCppDefMacroVirtual aclass =
  let funcDefStr = funcsToDefs aclass . virtualFuncs . class_funcs $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DEF_VIRT"
  in R.Define (R.sname macroname) (R.sname "Type") [ R.CVerbatim funcDefStr ]

---- "Class Definition NonVirtual" Declaration

genCppDefMacroNonVirtual :: Class -> R.CMacro
genCppDefMacroNonVirtual aclass =
  let funcDefStr = funcsToDefs aclass
                 . filter (not.isVirtualFunc)
                 . class_funcs
                 $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DEF_NONVIRT"
  in R.Define (R.sname macroname) (R.sname "Type") [ R.CVerbatim funcDefStr ]

---- Define Macro to provide Accessor C-C++ shim code for a class

genCppDefMacroAccessor :: Class -> R.CMacro
genCppDefMacroAccessor c =
  let funcDefStr = accessorsToDefs (class_vars c)
      macrocname = map toUpper (ffiClassName c)
      macroname = macrocname <> "_DEF_ACCESSOR"
  in R.Define (R.sname macroname) (R.sname "Type") [ R.CVerbatim funcDefStr ]

---- Define Macro to provide TemplateMemberFunction C-C++ shim code for a class

genCppDefMacroTemplateMemberFunction ::
     Class
  -> TemplateMemberFunction
  -> R.CMacro
genCppDefMacroTemplateMemberFunction c f =
   R.Define (R.sname macroname) (R.sname "Type")
     [ R.CVerbatim (subst tmpl ctxt) ]
  where
    macroname = hsTemplateMemberFunctionName c f
    tmpl = "extern \"C\" { \\\n\
           \  $decl; \\\n\
           \} \\\n\
           \inline $defn \\\n\
           \auto a_${macroname}_##Type = ${macroname}_##Type;\n"
    ctxt = context
             [ ("macroname", macroname)
             , ("decl"     , R.renderCDecl (tmplMemberFunToDecl c f))
             , ("defn"     , tmplMemberFunToDef c f)
             ]


---- Invoke Macro to define Virtual/NonVirtual method for a class

genCppDefInstVirtual :: (Class,Class) -> R.CStatement
genCppDefInstVirtual (p,c) =
  let macroname = map toUpper (ffiClassName p) <> "_DEF_VIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

  --  strc =  (ffiClassName p)
  -- in  strc<>"_DEF_VIRT(" <> ffiClassName c <> ")\n"

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

genAllCppHeaderInclude :: ClassImportHeader -> [R.CMacro]
genAllCppHeaderInclude header =
  map R.Include (cihIncludedHPkgHeadersInCPP header <> cihIncludedCPkgHeaders header)

----

-------------------------
-- TOP LEVEL FUNCTIONS --
-------------------------
-- genTopLevelFuncCppHeader

topLevelFunDecl :: TopLevelFunction -> R.CDecl
topLevelFunDecl TopLevelFunction {..} = R.FunDecl ret func args
  where
    ret  = R.CType (rettypeToString toplevelfunc_ret)
    func = R.sname ("TopLevel_" <> maybe toplevelfunc_name id toplevelfunc_alias)
    args = argsToCTypVarNoSelf toplevelfunc_args
topLevelFunDecl TopLevelVariable {..} = R.FunDecl ret func []
  where
    ret  = R.CType (rettypeToString toplevelvar_ret)
    func = R.sname ("TopLevel_" <> maybe toplevelvar_name id toplevelvar_alias)


genTopLevelFuncCppDefinition :: TopLevelFunction -> String
genTopLevelFuncCppDefinition tf@TopLevelFunction {..} =
  let tmpl = "$decl { \n  $funcbody\n}"
      decl = R.renderCDecl $ topLevelFunDecl tf
      callstr = toplevelfunc_name <> "("
                <> argsToCallString toplevelfunc_args
                <> ")"
      funcDefStr = returnCpp False (toplevelfunc_ret) callstr
  in subst tmpl (context [ ("decl"    , decl)
                         , ("funcbody", funcDefStr)
                         ]
                )
genTopLevelFuncCppDefinition tv@TopLevelVariable {..} =
  let tmpl = "$decl { \n  $funcbody\n}"
      decl = R.renderCDecl $ topLevelFunDecl tv
      callstr = toplevelvar_name
      funcDefStr = returnCpp False (toplevelvar_ret) callstr
  in subst tmpl (context [ ("decl"    , decl)
                         , ("funcbody", funcDefStr)
                         ]
                )


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

funcToDecl :: Class -> Function -> R.CDecl
funcToDecl c func
  | isNewFunc func || isStaticFunc func =
    let ret   = R.CType $ rettypeToString (genericFuncRet func)
        fname =
          R.CName [R.NamePart "Type", R.NamePart ("_" <> aliasedFuncName c func)]
        args  = argsToCTypVarNoSelf (genericFuncArgs func)
    in R.FunDecl ret fname args
  | otherwise =
    let ret   = R.CType $ rettypeToString (genericFuncRet func)
        fname =
          R.CName [R.NamePart "Type", R.NamePart ("_" <> aliasedFuncName c func)]
        args  = argsToCTypVar (genericFuncArgs func)
    in R.FunDecl ret fname args

funcsToDecls :: Class -> [Function] -> [R.CDecl]
funcsToDecls c = map (funcToDecl c)

  -- intercalateWith connSemicolonBSlash (funcToDecl c)


funcToDef :: Class -> Function -> String
funcToDef c func
  | isNewFunc func =
    let declstr = R.renderCDecl $ funcToDecl c func
        callstr = "(" <> argsToCallString (genericFuncArgs func) <> ")"
        returnstr = "Type * newp = new Type " <> callstr <> "; \\\nreturn to_nonconst<Type ## _t, Type >(newp);"
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"]
  | isDeleteFunc func =
    let declstr = R.renderCDecl $ funcToDecl c func
        returnstr = "delete (to_nonconst<Type,Type ## _t>(p)) ; "
    in  intercalateWith connBSlash id [declstr, "{", returnstr, "}"]
  | isStaticFunc func =
    let declstr = R.renderCDecl $ funcToDecl c func
        callstr = cppFuncName c func <> "("
                  <> argsToCallString (genericFuncArgs func)
                  <> ")"
        returnstr = returnCpp False (genericFuncRet func) callstr
    in intercalateWith connBSlash id [declstr, "{", returnstr, "}"]
  | otherwise =
    let declstr = R.renderCDecl $ funcToDecl c func
        callstr = "TYPECASTMETHOD(Type,"<> aliasedFuncName c func <> "," <> class_name c <> ")(p)->"
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

accessorToDecl :: Variable -> Accessor -> R.CDecl
accessorToDecl v a =
  let csig = accessorCFunSig (arg_type (unVariable v)) a
      ret = R.CType $ rettypeToString (cRetType csig)
      fname =
        R.CName [ R.NamePart "Type"
                , R.NamePart (   "_"
                              <> arg_name (unVariable v)
                              <> "_"
                              <> case a of Getter -> "get"; Setter -> "set"
                             )
                ]
      args = argsToCTypVar (cArgTypes csig)
  in R.FunDecl ret fname args

accessorsToDecls :: [Variable] -> [R.CDecl]
accessorsToDecls vs =
  concatMap (\v -> [accessorToDecl v Getter,accessorToDecl v Setter]) vs

accessorToDef :: Variable -> Accessor -> String
accessorToDef v a =
  let declstr = R.renderCDecl (accessorToDecl v a)
      varexp = "to_nonconst<Type,Type ## _t>(p)->" <> arg_name (unVariable v)
      body Getter = "return (" <> castCpp2C (arg_type (unVariable v)) varexp <> ");"
      body Setter =    varexp
                    <> " = "
                    <> castC2Cpp (arg_type (unVariable v)) "x"  -- TODO: somehow clean up this hard-coded "x".
                    <> ";"
  in intercalate "\\\n" [declstr, "{", body a, "}"]


accessorsToDefs :: [Variable] -> String
accessorsToDefs vs =
  let defs = concatMap (\v -> [accessorToDef v Getter,accessorToDef v Setter]) vs
  in intercalate "; \\\n" defs



-- Template Member Function Declaration and Definition

-- TODO: Handle simple type
tmplMemberFunToDecl :: Class -> TemplateMemberFunction -> R.CDecl
tmplMemberFunToDecl c f =
  let ret = R.CType $ tmplMemFuncRetTypeToString c (tmf_ret f)
      fname =
        R.CName [ R.NamePart (hsTemplateMemberFunctionName c f)
                , R.NamePart "Type"
                ]
      args = map (tmplMemFuncArgToCTypVar c) ((Arg SelfType "p"):tmf_args f)
  in R.FunDecl ret fname args

-- TODO: Handle simple type
tmplMemberFunToDef :: Class -> TemplateMemberFunction -> String
tmplMemberFunToDef c f =
    intercalateWith connBSlash id [ declstr
                                  , "  {"
                                  , "    " <> returnstr
                                  , "  }"
                                  ]
  where
    declstr = R.renderCDecl $ tmplMemberFunToDecl c f
    callstr =    "(to_nonconst<" <> ffiClassName c  <> "," <> ffiClassName c <> "_t" <> ">(p))"
              <> "->"
              <> tmf_name f
              <> "<Type>"
              <> "(" <> tmplAllArgsToCallString False (tmf_args f) <> ")"
    returnstr = returnCpp False (tmf_ret f) callstr
