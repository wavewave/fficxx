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
                                             , tmplAllArgsToCTypVar
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
import FFICXX.Generate.Util                  ( context
                                             , subst
                                             , toUppers
                                             )

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
    , R.TypeDef (R.CTVerbatim ("struct " <> classname_tag)) (R.sname classname_t)
    , R.TypeDef (R.CTVerbatim (classname_t <> " *"))        (R.sname classname_p)
    , R.TypeDef (R.CTVerbatim (classname_t <> " const*"))   (R.sname ("const_" <> classname_p))
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
                . map (funcToDecl aclass)
                . virtualFuncs
                . class_funcs
                $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DECL_VIRT"
  in R.Define (R.sname macroname) [R.sname "Type"] funcDecls

genCppHeaderMacroNonVirtual :: Class -> R.CMacro
genCppHeaderMacroNonVirtual c =
  let funcDecls = map R.CDeclaration
                . map (funcToDecl c)
                . filter (not.isVirtualFunc)
                . class_funcs
                $ c
      macrocname = map toUpper (ffiClassName c)
      macroname = macrocname <> "_DECL_NONVIRT"
  in R.Define (R.sname macroname) [R.sname "Type"] funcDecls


---- "Class Declaration Accessor" Declaration

genCppHeaderMacroAccessor :: Class -> R.CMacro
genCppHeaderMacroAccessor c =
  let funcDecls  = map R.CDeclaration $ accessorsToDecls (class_vars c)
      macrocname = map toUpper (ffiClassName c)
      macroname  = macrocname <> "_DECL_ACCESSOR"
  in R.Define (R.sname macroname) [R.sname "Type"] funcDecls


---- "Class Declaration Virtual/NonVirtual/Accessor" Instances

genCppHeaderInstVirtual :: (Class,Class) -> R.CStatement
genCppHeaderInstVirtual (p,c) =
  let macroname = map toUpper (ffiClassName p) <> "_DECL_VIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppHeaderInstNonVirtual :: Class -> R.CStatement
genCppHeaderInstNonVirtual c =
  let macroname = map toUpper (ffiClassName c) <> "_DECL_NONVIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppHeaderInstAccessor :: Class -> R.CStatement
genCppHeaderInstAccessor c =
  let macroname = map toUpper (ffiClassName c) <> "_DECL_ACCESSOR"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefMacroVirtual :: Class -> R.CMacro
genCppDefMacroVirtual aclass =
  let funcDefStr = intercalate "\n"
                 . map (R.renderCStmt . funcToDef aclass)
                 . virtualFuncs
                 . class_funcs
                 $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DEF_VIRT"
  in R.Define (R.sname macroname) [R.sname "Type"] [ R.CVerbatim funcDefStr ]

---- "Class Definition NonVirtual" Declaration

genCppDefMacroNonVirtual :: Class -> R.CMacro
genCppDefMacroNonVirtual aclass =
  let funcDefStr = intercalate "\n"
                 . map (R.renderCStmt . funcToDef aclass)
                 . filter (not.isVirtualFunc)
                 . class_funcs
                 $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DEF_NONVIRT"
  in R.Define (R.sname macroname) [R.sname "Type"] [ R.CVerbatim funcDefStr ]

---- Define Macro to provide Accessor C-C++ shim code for a class

genCppDefMacroAccessor :: Class -> R.CMacro
genCppDefMacroAccessor c =
  let funcDefStr = accessorsToDefs (class_vars c)
      macrocname = map toUpper (ffiClassName c)
      macroname = macrocname <> "_DEF_ACCESSOR"
  in R.Define (R.sname macroname) [R.sname "Type"] [ R.CVerbatim funcDefStr ]

---- Define Macro to provide TemplateMemberFunction C-C++ shim code for a class

genCppDefMacroTemplateMemberFunction ::
     Class
  -> TemplateMemberFunction
  -> R.CMacro
genCppDefMacroTemplateMemberFunction c f =
   R.Define (R.sname macroname) [R.sname "Type"]
     [ R.CVerbatim (subst tmpl ctxt) ]
  where
    macroname = hsTemplateMemberFunctionName c f
    tmpl = "extern \"C\" { \n\
           \  $decl; \n\
           \} \n\
           \inline $defn \n\
           \auto a_${macroname}_##Type = ${macroname}_##Type;\n"
    ctxt = context
             [ ("macroname", macroname)
             , ("decl"     , R.renderCDecl (tmplMemberFunToDecl c f))
             , ("defn"     , R.renderCStmt (tmplMemberFunToDef c f))
             ]


---- Invoke Macro to define Virtual/NonVirtual method for a class

genCppDefInstVirtual :: (Class,Class) -> R.CStatement
genCppDefInstVirtual (p,c) =
  let macroname = map toUpper (ffiClassName p) <> "_DEF_VIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppDefInstNonVirtual :: Class -> R.CStatement
genCppDefInstNonVirtual c =
  let macroname = toUppers (ffiClassName c) <> "_DEF_NONVIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppDefInstAccessor :: Class -> R.CStatement
genCppDefInstAccessor c =
  let macroname = toUppers (ffiClassName c) <> "_DEF_ACCESSOR"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

-----------------

genAllCppHeaderInclude :: ClassImportHeader -> [R.CMacro]
genAllCppHeaderInclude header =
  map R.Include (cihIncludedHPkgHeadersInCPP header <> cihIncludedCPkgHeaders header)

----

-------------------------
-- TOP LEVEL FUNCTIONS --
-------------------------

topLevelFunDecl :: TopLevelFunction -> R.CDecl
topLevelFunDecl TopLevelFunction {..} = R.FunDecl ret func args
  where
    ret  = R.CTVerbatim (rettypeToString toplevelfunc_ret)
    func = R.sname ("TopLevel_" <> maybe toplevelfunc_name id toplevelfunc_alias)
    args = argsToCTypVarNoSelf toplevelfunc_args
topLevelFunDecl TopLevelVariable {..} = R.FunDecl ret func []
  where
    ret  = R.CTVerbatim (rettypeToString toplevelvar_ret)
    func = R.sname ("TopLevel_" <> maybe toplevelvar_name id toplevelvar_alias)

genTopLevelFuncCppDefinition :: TopLevelFunction -> R.CStatement
genTopLevelFuncCppDefinition tf@TopLevelFunction {..} =
  let decl = topLevelFunDecl tf
      callstr = toplevelfunc_name <> "("
                <> argsToCallString toplevelfunc_args
                <> ")"
      body = returnCpp False (toplevelfunc_ret) callstr
  in R.CDefinition decl body
genTopLevelFuncCppDefinition tv@TopLevelVariable {..} =
  let decl = topLevelFunDecl tv
      callstr = toplevelvar_name
      body = returnCpp False (toplevelvar_ret) callstr
  in R.CDefinition decl body

genTmplFunCpp :: Bool -- ^ is for simple type?
              -> TemplateClass
              -> TemplateFunction
              -> R.CMacro
genTmplFunCpp b t@TmplCls {..} f =
    R.Define (R.sname macroname) [R.sname "Type"] [R.CVerbatim defn]
 where
  suffix = if b then "_s" else ""
  macroname = tclass_name <> "_" <> ffiTmplFuncName f <> suffix
  defn = subst tmpl ctxt
  tmpl = "extern \"C\" { \n\
         \  $decl; \n\
         \} \n\
         \inline $defn \n\
         \auto a_${tname}_${fname}_ ## Type = ${tname}_${fname}_ ## Type;\n"

  ctxt = context
           [ ("tname"  , tclass_name )
           , ("fname"  , ffiTmplFuncName f)
           , ("decl"   , R.renderCDecl (tmplFunToDecl b t f) )
           , ("defn"   , R.renderCStmt (tmplFunToDef b t f) )
           ]

genTmplClassCpp :: Bool -- ^ is for simple type
                -> TemplateClass
                -> [TemplateFunction]
                -> R.CMacro
genTmplClassCpp b TmplCls {..} fs =
    R.Define (R.sname macroname) [R.sname "Type"] [R.CVerbatim macro]
 where
  suffix = if b then "_s" else ""
  tname = tclass_name
  macroname = tname <> "_instance" <> suffix
  macro1 f@TFun {..}    = "  " <> tname<> "_" <> ffiTmplFuncName f <> suffix <> "(Type)"

  macro1 f@TFunNew {..} = "  " <> tname<> "_" <> ffiTmplFuncName f <> "(Type)"
  macro1 TFunDelete     = "  " <> tname<> "_delete(Type)"
  macro = intercalate "\n" $ map macro1 fs

returnCpp :: Bool  -- ^ for simple type
          -> Types
          -> String -- ^ call string
          -> [R.CStatement]
returnCpp b ret callstr =
  case ret of
    Void                    -> [R.CVerbatim (callstr <> ";")]
    SelfType                -> [R.CReturn $ R.CEVerbatim $ "to_nonconst<Type ## _t, Type>((Type *)" <> callstr <> ")"]
    CT (CRef _) _           -> [R.CReturn $ R.CEVerbatim $ "(&("<>callstr<>"))"]
    CT _ _                  -> [R.CReturn $ R.CEVerbatim $ callstr]
    CPT (CPTClass c') _     -> [R.CReturn $ R.CEVerbatim $ "to_nonconst<"<>str<>"_t,"<>str<>">(("<>str<>"*)"<>callstr<>")"]
                               where str = ffiClassName c'
    CPT (CPTClassRef c') _  -> [R.CReturn $ R.CEVerbatim $ "to_nonconst<"<>str<>"_t,"<>str<>">(&("<>callstr<>"))"]
                               where str = ffiClassName c'
    CPT (CPTClassCopy c') _ -> [R.CReturn $ R.CEVerbatim $ "to_nonconst<"<>str<>"_t,"<>str<>">(new "<>str<>"("<>callstr<>"))"]
                               where str = ffiClassName c'
    CPT (CPTClassMove c') _ -> -- TODO: check whether this is working or not.
                               [R.CReturn $ R.CEVerbatim $ "std::move(to_nonconst<"<>str<>"_t,"<>str<>">(&("<>callstr<>")))"]
                               where str = ffiClassName c'
    TemplateApp (TemplateAppInfo _ _ cpptype) ->
      [ R.CVerbatim $ cpptype <> "* r = new " <> cpptype <> "(" <> callstr <> ");"
      , R.CReturn $ R.CEVerbatim "(static_cast<void*>(r))"
      ]
    TemplateAppRef (TemplateAppInfo _ _ cpptype) ->
      [ R.CVerbatim $ cpptype <> "* r = new " <> cpptype <> "(" <> callstr <> ");"
      , R.CReturn $ R.CEVerbatim "(static_cast<void*>(r))"
      ]
    TemplateAppMove (TemplateAppInfo _ _ cpptype) ->
      [ R.CVerbatim $ cpptype <> "* r = new " <> cpptype <> "(" <> callstr <> ");"
      , R.CReturn $ R.CEVerbatim "std::move(static_cast<void*>(r));"
      ]
    TemplateType _          -> error "returnCpp: TemplateType"
    TemplateParam _         ->
      [ R.CReturn $ R.CEVerbatim $
          if b then "(" <> callstr <> ")"
               else "to_nonconst<Type ## _t, Type>((Type *)&(" <> callstr <> "))"
      ]
    TemplateParamPointer _  ->
      [ R.CReturn $ R.CEVerbatim $
          if b then "(" <> callstr <> ")"
               else "to_nonconst<Type ## _t, Type>(" <> callstr <> ")"
      ]

-- Function Declaration and Definition

funcToDecl :: Class -> Function -> R.CDecl
funcToDecl c func
  | isNewFunc func || isStaticFunc func =
    let ret   = R.CTVerbatim $ rettypeToString (genericFuncRet func)
        fname =
          R.CName [R.NamePart "Type", R.NamePart ("_" <> aliasedFuncName c func)]
        args  = argsToCTypVarNoSelf (genericFuncArgs func)
    in R.FunDecl ret fname args
  | otherwise =
    let ret   = R.CTVerbatim $ rettypeToString (genericFuncRet func)
        fname =
          R.CName [R.NamePart "Type", R.NamePart ("_" <> aliasedFuncName c func)]
        args  = argsToCTypVar (genericFuncArgs func)
    in R.FunDecl ret fname args

funcToDef :: Class -> Function -> R.CStatement
funcToDef c func
  | isNewFunc func =
    let callstr = "(" <> argsToCallString (genericFuncArgs func) <> ")"
        body = [ R.CVerbatim $ "Type * newp = new Type " <> callstr <> ";"
               , R.CReturn $ R.CEVerbatim "to_nonconst<Type ## _t, Type >(newp);"
               ]
    in R.CDefinition (funcToDecl c func) body
  | isDeleteFunc func =
    let body = [ R.CVerbatim $ "delete (to_nonconst<Type,Type ## _t>(p));" ]
    in R.CDefinition (funcToDecl c func) body
  | isStaticFunc func =
    let callstr = cppFuncName c func <> "("
                  <> argsToCallString (genericFuncArgs func)
                  <> ")"
        body = returnCpp False (genericFuncRet func) callstr
    in R.CDefinition (funcToDecl c func) body
  | otherwise =
    let callstr = "TYPECASTMETHOD(Type,"<> aliasedFuncName c func <> "," <> class_name c <> ")(p)->"
                  <> cppFuncName c func <> "("
                  <> argsToCallString (genericFuncArgs func)
                  <> ")"
        body = returnCpp False (genericFuncRet func) callstr
    in R.CDefinition (funcToDecl c func) body

tmplFunToDecl :: Bool -> TemplateClass -> TemplateFunction -> R.CDecl
tmplFunToDecl b t@TmplCls {..} f@TFun {..}    = R.FunDecl ret func args
  where
    ret  = R.CTVerbatim (tmplRetTypeToString b tfun_ret)
    func = R.CName [R.NamePart (tclass_name <> "_" <> ffiTmplFuncName f <> "_"), R.NamePart "Type"]
    args = tmplAllArgsToCTypVar b Self t tfun_args
tmplFunToDecl b t@TmplCls {..} f@TFunNew {..} = R.FunDecl ret func args
  where
    ret  = R.CTVerbatim (tmplRetTypeToString b (TemplateType t))
    func = R.CName [R.NamePart (tclass_name <> "_" <> ffiTmplFuncName f <> "_"), R.NamePart "Type"]
    args = tmplAllArgsToCTypVar b NoSelf t tfun_new_args
tmplFunToDecl b t@TmplCls {..} TFunDelete     = R.FunDecl ret func args
  where
    ret  = R.CTVerbatim "void"
    func = R.CName [R.NamePart (tclass_name <> "_delete_"), R.NamePart "Type"]
    args = tmplAllArgsToCTypVar b Self t []

tmplFunToDef :: Bool -- ^ for simple type
             -> TemplateClass
             -> TemplateFunction
             -> R.CStatement
tmplFunToDef b t@TmplCls {..} f =
    R.CDefinition (tmplFunToDecl b t f) body
  where
    callstr =
      case f of
        TFun {..} ->
             "(static_cast<" <> tclass_oname <> "<Type>*>(p))->"
          <> tfun_oname <> "("
          <> tmplAllArgsToCallString b tfun_args
          <> ")"
        TFunNew {..} ->
             "new " <> tclass_oname <> "<Type>("
          <> tmplAllArgsToCallString b tfun_new_args
          <> ")"
        TFunDelete ->
          "delete (static_cast<" <> tclass_oname <> "<Type>*>(p))"
    body =
      case f of
        TFunNew {..} -> [ R.CReturn $ R.CEVerbatim $ "static_cast<void*>("<>callstr<>")" ]
        TFunDelete   -> [ R.CReturn $ R.CEVerbatim $ callstr ]
        TFun {..}    -> returnCpp b (tfun_ret) callstr

-- Accessor Declaration and Definition

accessorToDecl :: Variable -> Accessor -> R.CDecl
accessorToDecl v a =
  let csig = accessorCFunSig (arg_type (unVariable v)) a
      ret = R.CTVerbatim $ rettypeToString (cRetType csig)
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

accessorToDef :: Variable -> Accessor -> R.CStatement
accessorToDef v a =
  let varexp = "to_nonconst<Type,Type ## _t>(p)->" <> arg_name (unVariable v)
      body Getter = R.CReturn $ R.CEVerbatim $ "(" <> castCpp2C (arg_type (unVariable v)) varexp <> ")"
      body Setter = R.CVerbatim $
                         varexp
                      <> " = "
                      <> castC2Cpp (arg_type (unVariable v)) "x"  -- TODO: clean up this hard-coded "x".
                      <> ";"
  in R.CDefinition (accessorToDecl v a) [ body a ]

accessorsToDefs :: [Variable] -> String
accessorsToDefs vs =
  let defs = concatMap (\v -> [accessorToDef v Getter,accessorToDef v Setter]) vs
  in intercalate "; \n" $ map R.renderCStmt defs

-- Template Member Function Declaration and Definition

-- TODO: Handle simple type
tmplMemberFunToDecl :: Class -> TemplateMemberFunction -> R.CDecl
tmplMemberFunToDecl c f =
  let ret = R.CTVerbatim $ tmplMemFuncRetTypeToString c (tmf_ret f)
      fname =
        R.CName [ R.NamePart (hsTemplateMemberFunctionName c f)
                , R.NamePart "Type"
                ]
      args = map (tmplMemFuncArgToCTypVar c) ((Arg SelfType "p"):tmf_args f)
  in R.FunDecl ret fname args

-- TODO: Handle simple type
tmplMemberFunToDef :: Class -> TemplateMemberFunction -> R.CStatement
tmplMemberFunToDef c f =
    R.CDefinition (tmplMemberFunToDecl c f) body
  where
    callstr =    "(to_nonconst<" <> ffiClassName c  <> "," <> ffiClassName c <> "_t" <> ">(p))"
              <> "->"
              <> tmf_name f
              <> "<Type>"
              <> "(" <> tmplAllArgsToCallString False (tmf_args f) <> ")"
    body = returnCpp False (tmf_ret f) callstr
