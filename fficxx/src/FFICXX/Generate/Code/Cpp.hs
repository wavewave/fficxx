{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FFICXX.Generate.Code.Cpp where

import Data.Char             ( toUpper )
import Data.Functor.Identity ( Identity )
import Data.List             ( intercalate, intersperse )
import Data.Monoid           ( (<>) )
--
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import FFICXX.Runtime.TH     ( IsCPrimitive(CPrim, NonCPrim) )
--
import FFICXX.Generate.Code.Primitive
                                    ( accessorCFunSig
                                    , argToCallCExp
                                    , argsToCTypVar
                                    , argsToCTypVarNoSelf
                                    , c2Cxx
                                    , cxx2C
                                    , CFunSig(..)
                                    , genericFuncArgs
                                    , genericFuncRet
                                    , returnCType
                                    , tmplMemFuncArgToCTypVar
                                    , tmplMemFuncReturnCType
                                    , tmplAllArgsToCTypVar
                                    , tmplAppTypeFromForm
                                    , tmplArgToCallCExp
                                    , tmplReturnCType
                                    )
import FFICXX.Generate.Name         ( aliasedFuncName
                                    , cppFuncName
                                    , ffiClassName
                                    , ffiTmplFuncName
                                    , hsTemplateMemberFunctionName
                                    )
import FFICXX.Generate.Type.Class   ( Accessor(Getter,Setter)
                                    , Arg(..)
                                    , Class(..)
                                    , CPPTypes(..)
                                    , CTypes(..)
                                    , Form(FormSimple,FormNested)
                                    , Function(..)
                                    , Selfness(NoSelf,Self)
                                    , TemplateAppInfo(..)
                                    , TemplateClass(..)
                                    , TemplateFunction(..)
                                    , TemplateMemberFunction(..)
                                    , TopLevelFunction(..)
                                    , Types(..)
                                    , Variable(unVariable)
                                    , argsFromOpExp
                                    , isDeleteFunc
                                    , isNewFunc
                                    , isStaticFunc
                                    , isVirtualFunc
                                    , opSymbol
                                    , virtualFuncs
                                    )
import FFICXX.Generate.Type.Module  ( ClassImportHeader(..) )
import FFICXX.Generate.Util         ( toUppers )

--
--
-- Class Declaration and Definition
--

----
---- Declaration
----

---- "Class Type Declaration" Instances

typedefStmts :: String -> [R.CStatement Identity]
typedefStmts classname =
    [ R.TypeDef (R.CTVerbatim ("struct " <> classname_tag)) (R.sname classname_t)
    , R.TypeDef (R.CTVerbatim (classname_t <> " *"))        (R.sname classname_p)
    , R.TypeDef (R.CTVerbatim (classname_t <> " const*"))   (R.sname ("const_" <> classname_p))
    ]
  where
    classname_tag = classname <> "_tag"
    classname_t   = classname <> "_t"
    classname_p   = classname <> "_p"


genCppHeaderMacroType :: Class -> [R.CStatement Identity]
genCppHeaderMacroType c =
    [ R.Comment "Opaque type definition for $classname" ]
    <> typedefStmts (ffiClassName c)


---- "Class Declaration Virtual" Declaration

genCppHeaderMacroVirtual :: Class -> R.CMacro Identity
genCppHeaderMacroVirtual aclass =
  let funcDecls = map R.CDeclaration
                . map (funcToDecl aclass)
                . virtualFuncs
                . class_funcs
                $ aclass
      macrocname = map toUpper (ffiClassName aclass)
      macroname = macrocname <> "_DECL_VIRT"
  in R.Define (R.sname macroname) [R.sname "Type"] funcDecls

genCppHeaderMacroNonVirtual :: Class -> R.CMacro Identity
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

genCppHeaderMacroAccessor :: Class -> R.CMacro Identity
genCppHeaderMacroAccessor c =
  let funcDecls  = map R.CDeclaration $ accessorsToDecls (class_vars c)
      macrocname = map toUpper (ffiClassName c)
      macroname  = macrocname <> "_DECL_ACCESSOR"
  in R.Define (R.sname macroname) [R.sname "Type"] funcDecls


---- "Class Declaration Virtual/NonVirtual/Accessor" Instances

genCppHeaderInstVirtual :: (Class,Class) -> R.CStatement Identity
genCppHeaderInstVirtual (p,c) =
  let macroname = map toUpper (ffiClassName p) <> "_DECL_VIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppHeaderInstNonVirtual :: Class -> R.CStatement Identity
genCppHeaderInstNonVirtual c =
  let macroname = map toUpper (ffiClassName c) <> "_DECL_NONVIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppHeaderInstAccessor :: Class -> R.CStatement Identity
genCppHeaderInstAccessor c =
  let macroname = map toUpper (ffiClassName c) <> "_DECL_ACCESSOR"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefMacroVirtual :: Class -> R.CMacro Identity
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

genCppDefMacroNonVirtual :: Class -> R.CMacro Identity
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

genCppDefMacroAccessor :: Class -> R.CMacro Identity
genCppDefMacroAccessor c =
  let funcDefs = concatMap (\v -> [accessorToDef v Getter,accessorToDef v Setter]) (class_vars c)
      macrocname = map toUpper (ffiClassName c)
      macroname = macrocname <> "_DEF_ACCESSOR"
  in R.Define (R.sname macroname) [R.sname "Type"] funcDefs

---- Define Macro to provide TemplateMemberFunction C-C++ shim code for a class

genCppDefMacroTemplateMemberFunction ::
     Class
  -> TemplateMemberFunction
  -> R.CMacro Identity
genCppDefMacroTemplateMemberFunction c f =
   R.Define (R.sname macroname) (map R.sname (tmf_params f))
     [ R.CExtern [R.CDeclaration decl]
     , tmplMemberFunToDef c f
     , autoinst
     ]
  where
    nsuffix = intersperse (R.NamePart "_") $ map R.NamePart (tmf_params f)
    macroname = hsTemplateMemberFunctionName c f
    decl = tmplMemberFunToDecl c f
    autoinst =
      R.CInit
        (R.CVarDecl
          R.CTAuto
          (R.CName (R.NamePart ("a_" <> macroname <> "_") : nsuffix))
        )
        (R.CVar (R.CName (R.NamePart (macroname <> "_") : nsuffix)))


---- Invoke Macro to define Virtual/NonVirtual method for a class

genCppDefInstVirtual :: (Class, Class) -> R.CStatement Identity
genCppDefInstVirtual (p,c) =
  let macroname = map toUpper (ffiClassName p) <> "_DEF_VIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppDefInstNonVirtual :: Class -> R.CStatement Identity
genCppDefInstNonVirtual c =
  let macroname = toUppers (ffiClassName c) <> "_DEF_NONVIRT"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

genCppDefInstAccessor :: Class -> R.CStatement Identity
genCppDefInstAccessor c =
  let macroname = toUppers (ffiClassName c) <> "_DEF_ACCESSOR"
  in R.CMacroApp (R.sname macroname) [R.sname (ffiClassName c)]

-----------------

genAllCppHeaderInclude :: ClassImportHeader -> [R.CMacro Identity]
genAllCppHeaderInclude header =
  map R.Include (cihIncludedHPkgHeadersInCPP header <> cihIncludedCPkgHeaders header)

----

-------------------------
-- TOP LEVEL FUNCTIONS --
-------------------------

topLevelFunDecl :: TopLevelFunction -> R.CFunDecl Identity
topLevelFunDecl TopLevelFunction {..} = R.CFunDecl ret func args
  where
    ret  = returnCType toplevelfunc_ret
    func = R.sname ("TopLevel_" <> maybe toplevelfunc_name id toplevelfunc_alias)
    args = argsToCTypVarNoSelf toplevelfunc_args
topLevelFunDecl TopLevelVariable {..} = R.CFunDecl ret func []
  where
    ret  = returnCType toplevelvar_ret
    func = R.sname ("TopLevel_" <> maybe toplevelvar_name id toplevelvar_alias)

genTopLevelFuncCppDefinition :: TopLevelFunction -> R.CStatement Identity
genTopLevelFuncCppDefinition tf@TopLevelFunction {..} =
  let decl = topLevelFunDecl tf
      body = returnCpp
               NonCPrim
               (toplevelfunc_ret)
               (R.CApp (R.CVar (R.sname toplevelfunc_name)) (map argToCallCExp toplevelfunc_args))
  in R.CDefinition Nothing decl body
genTopLevelFuncCppDefinition tv@TopLevelVariable {..} =
  let decl = topLevelFunDecl tv
      body = returnCpp NonCPrim (toplevelvar_ret) (R.CVar (R.sname toplevelvar_name))
  in R.CDefinition Nothing decl body

genTmplFunCpp ::
     IsCPrimitive
  -> TemplateClass
  -> TemplateFunction
  -> R.CMacro Identity
genTmplFunCpp b t@TmplCls {..} f =
    R.Define (R.sname macroname) (map R.sname tclass_params)
      [ R.CExtern [R.CDeclaration decl]
      , tmplFunToDef b t f
      , autoinst
      ]
 where
  nsuffix = intersperse (R.NamePart "_") $ map R.NamePart tclass_params
  suffix = case b of { CPrim -> "_s"; NonCPrim -> "" }
  macroname = tclass_name <> "_" <> ffiTmplFuncName f <> suffix
  decl = tmplFunToDecl b t f
  autoinst =
    R.CInit
      (R.CVarDecl
        R.CTAuto
        (R.CName (R.NamePart ("a_" <> tclass_name <> "_" <> ffiTmplFuncName f <> "_") : nsuffix ))
      )
      (R.CVar (R.CName (R.NamePart (tclass_name <> "_" <> ffiTmplFuncName f <> "_") : nsuffix )))

-- |
genTmplClassCpp ::
     IsCPrimitive
  -> TemplateClass
  -> [TemplateFunction]
  -> R.CMacro Identity
genTmplClassCpp b TmplCls {..} fs =
    R.Define (R.sname macroname) params (map macro1 fs)
 where
  params = map R.sname tclass_params
  suffix = case b of { CPrim -> "_s"; NonCPrim -> "" }
  tname = tclass_name
  macroname = tname <> "_instance" <> suffix
  macro1 f@TFun {..}    = R.CMacroApp (R.sname (tname <> "_" <> ffiTmplFuncName f <> suffix)) params
  macro1 f@TFunNew {..} = R.CMacroApp (R.sname (tname <> "_" <> ffiTmplFuncName f <> suffix)) params
  macro1 TFunDelete     = R.CMacroApp (R.sname (tname <> "_delete" <> suffix)) params
  macro1 f@TFunOp {..}  = R.CMacroApp (R.sname (tname <> "_" <> ffiTmplFuncName f <> suffix)) params

-- |
returnCpp ::
     IsCPrimitive
  -> Types
  -> R.CExp Identity
  -> [R.CStatement Identity]
returnCpp b ret caller =
  case ret of
    Void ->
      [ R.CExpSA caller ]
    SelfType ->
      [R.CReturn $
        R.CTApp
          (R.sname "to_nonconst")
          [ R.CTSimple (R.CName [ R.NamePart "Type", R.NamePart "_t" ])
          , R.CTSimple (R.sname "Type") ]
          [ R.CCast (R.CTStar (R.CTSimple (R.sname "Type"))) caller ]
      ]
    CT (CRef _) _ ->
      [R.CReturn $ R.CAddr caller ]
    CT _ _ ->
      [R.CReturn caller ]
    CPT (CPTClass c') _ ->
      [R.CReturn $
        R.CTApp
          (R.sname "to_nonconst")
          [ R.CTSimple (R.sname (str <> "_t")), R.CTSimple (R.sname str) ]
          [ R.CCast (R.CTStar (R.CTSimple (R.sname str))) caller ]
      ]
      where str = ffiClassName c'
    CPT (CPTClassRef c') _ ->
      [R.CReturn $
        R.CTApp
          (R.sname "to_nonconst")
          [ R.CTSimple (R.sname (str <> "_t")), R.CTSimple (R.sname str) ]
          [ R.CAddr caller ]
      ]
      where str = ffiClassName c'
    CPT (CPTClassCopy c') _ ->
      [R.CReturn $
        R.CTApp
          (R.sname "to_nonconst")
          [ R.CTSimple (R.sname (str <> "_t")), R.CTSimple (R.sname str) ]
          [ R.CNew (R.sname str) [ caller ]  ]
      ]
      where str = ffiClassName c'
    CPT (CPTClassMove c') _ -> -- TODO: check whether this is working or not.
      [R.CReturn $
        R.CApp
          (R.CVar (R.sname "std::move"))
          [R.CTApp
            (R.sname "to_nonconst")
            [ R.CTSimple (R.sname (str <> "_t")), R.CTSimple (R.sname str) ]
            [ R.CAddr caller ]
          ]
      ]
      where str = ffiClassName c'
    TemplateApp (TemplateAppInfo _ _ cpptype) ->
      [ R.CInit
          (R.CVarDecl (R.CTStar (R.CTVerbatim cpptype)) (R.sname "r"))
          (R.CNew (R.sname cpptype) [ caller ])
      , R.CReturn $
          R.CTApp
            (R.sname "static_cast")
            [ R.CTStar R.CTVoid ]
            [ R.CVar (R.sname "r") ]
      ]
    TemplateAppRef (TemplateAppInfo _ _ cpptype) ->
      [ R.CInit
          (R.CVarDecl (R.CTStar (R.CTVerbatim cpptype)) (R.sname "r"))
          (R.CNew (R.sname cpptype) [ caller ])
      , R.CReturn $
          R.CTApp
            (R.sname "static_cast")
            [ R.CTStar R.CTVoid ]
            [ R.CVar (R.sname "r") ]
      ]
    TemplateAppMove (TemplateAppInfo _ _ cpptype) ->
      [ R.CInit
          (R.CVarDecl (R.CTStar (R.CTVerbatim cpptype)) (R.sname "r"))
          (R.CNew (R.sname cpptype) [ caller ])
      , R.CReturn $
          R.CApp
            (R.CVar (R.sname "std::move"))
            [R.CTApp
              (R.sname "staic_cast")
              [ R.CTStar R.CTVoid ]
              [ R.CVar (R.sname "r") ]
            ]
      ]
    TemplateType _ ->
      error "returnCpp: TemplateType"
    TemplateParam typ ->
      [ R.CReturn $
          case b of
            CPrim    -> caller
            NonCPrim ->
              R.CTApp
                (R.sname "to_nonconst")
                [ R.CTSimple (R.CName [ R.NamePart typ, R.NamePart "_t" ]), R.CTSimple (R.sname typ) ]
                [ R.CCast (R.CTStar (R.CTSimple (R.sname typ))) $ R.CAddr caller ]
      ]
    TemplateParamPointer typ ->
      [ R.CReturn $
          case b of
            CPrim    -> caller
            NonCPrim ->
              R.CTApp
                (R.sname "to_nonconst")
                [ R.CTSimple (R.CName [ R.NamePart typ, R.NamePart "_t"]), R.CTSimple (R.sname typ) ]
                [ caller ]
      ]

-- Function Declaration and Definition

funcToDecl :: Class -> Function -> R.CFunDecl Identity
funcToDecl c func
  | isNewFunc func || isStaticFunc func =
    let ret   = returnCType (genericFuncRet func)
        fname =
          R.CName [R.NamePart "Type", R.NamePart ("_" <> aliasedFuncName c func)]
        args  = argsToCTypVarNoSelf (genericFuncArgs func)
    in R.CFunDecl ret fname args
  | otherwise =
    let ret   = returnCType (genericFuncRet func)
        fname =
          R.CName [R.NamePart "Type", R.NamePart ("_" <> aliasedFuncName c func)]
        args  = argsToCTypVar (genericFuncArgs func)
    in R.CFunDecl ret fname args

funcToDef :: Class -> Function -> R.CStatement Identity
funcToDef c func
  | isNewFunc func =
    let body = [ R.CInit
                   (R.CVarDecl (R.CTStar (R.CTSimple (R.sname "Type"))) (R.sname "newp"))
                   (R.CNew (R.sname "Type") $ map argToCallCExp (genericFuncArgs func))
               , R.CReturn $
                   R.CTApp
                     (R.sname "to_nonconst")
                     [ R.CTSimple (R.CName [ R.NamePart "Type", R.NamePart "_t"]), R.CTSimple (R.sname "Type") ]
                     [ R.CVar (R.sname "newp") ]
               ]
    in R.CDefinition Nothing (funcToDecl c func) body
  | isDeleteFunc func =
    let body = [ R.CDelete $
                   R.CTApp
                     (R.sname "to_nonconst")
                     [ R.CTSimple (R.sname "Type"), R.CTSimple (R.CName [ R.NamePart "Type", R.NamePart "_t" ]) ]
                     [ R.CVar (R.sname "p") ]
               ]
    in R.CDefinition Nothing (funcToDecl c func) body
  | isStaticFunc func =
    let body = returnCpp NonCPrim (genericFuncRet func) $
                 R.CApp (R.CVar (R.sname (cppFuncName c func))) (map argToCallCExp (genericFuncArgs func))
    in R.CDefinition Nothing (funcToDecl c func) body
  | otherwise =
    let caller =
          R.CBinOp
            R.CArrow
            (R.CApp
              (R.CEMacroApp
                (R.sname "TYPECASTMETHOD")
                [ R.sname "Type", R.sname (aliasedFuncName c func), R.sname (class_name c) ]
              )
              [ R.CVar (R.sname "p") ]
            )
            (R.CApp (R.CVar (R.sname (cppFuncName c func))) (map argToCallCExp (genericFuncArgs func)))
        body = returnCpp NonCPrim (genericFuncRet func) caller
    in R.CDefinition Nothing (funcToDecl c func) body

-- template function declaration and definition


tmplFunToDecl ::
     IsCPrimitive
  -> TemplateClass
  -> TemplateFunction
  -> R.CFunDecl Identity
tmplFunToDecl b t@TmplCls {..} f =
  let nsuffix = intersperse (R.NamePart "_") $ map R.NamePart tclass_params
  in case f of
    TFun {..} ->
      let ret  = tmplReturnCType b tfun_ret
          func = R.CName (R.NamePart (tclass_name <> "_" <> ffiTmplFuncName f <> "_") : nsuffix)
          args = tmplAllArgsToCTypVar b Self t tfun_args
      in R.CFunDecl ret func args
    TFunNew {..} ->
      let ret  = tmplReturnCType b (TemplateType t)
          func = R.CName (R.NamePart (tclass_name <> "_" <> ffiTmplFuncName f <> "_") : nsuffix)
          args = tmplAllArgsToCTypVar b NoSelf t tfun_new_args
      in R.CFunDecl ret func args
    TFunDelete ->
      let ret  = R.CTVoid
          func = R.CName (R.NamePart (tclass_name <> "_delete_") : nsuffix)
          args = tmplAllArgsToCTypVar b Self t []
      in R.CFunDecl ret func args
    TFunOp {..} ->
      let ret  = tmplReturnCType b tfun_ret
          func = R.CName (R.NamePart (tclass_name <> "_" <> ffiTmplFuncName f <> "_") : nsuffix)
          args = tmplAllArgsToCTypVar b Self t (argsFromOpExp tfun_opexp)
      in R.CFunDecl ret func args

-- |
tmplFunToDef ::
     IsCPrimitive
  -> TemplateClass
  -> TemplateFunction
  -> R.CStatement Identity
tmplFunToDef b t@TmplCls {..} f =
    R.CDefinition (Just R.Inline) (tmplFunToDecl b t f) body
  where
    typparams = map (R.CTSimple . R.sname) tclass_params
    body =
      case f of
        TFunNew {..} ->
          let caller =
                case tclass_cxxform of
                  FormSimple tclass ->
                    R.CTNew
                      (R.sname tclass)
                      typparams
                      (map (tmplArgToCallCExp b) tfun_new_args)
                  FormNested tclass inner ->
                    R.CTNewI
                      (R.sname tclass)
                      (R.sname inner)
                      typparams
                      (map (tmplArgToCallCExp b) tfun_new_args)
          in  [ R.CReturn $ R.CTApp (R.sname "static_cast") [R.CTStar R.CTVoid] [caller] ]
        TFunDelete ->
          [ R.CDelete $
              R.CTApp
                (R.sname "static_cast")
                [ R.CTStar $ tmplAppTypeFromForm tclass_cxxform typparams ]
                [ R.CVar (R.sname "p") ]
          ]
        TFun {..}    ->
          returnCpp b (tfun_ret) $
            R.CBinOp
              R.CArrow
              (R.CTApp
                 (R.sname "static_cast")
                 [ R.CTStar $ tmplAppTypeFromForm tclass_cxxform typparams ]
                 [ R.CVar $ R.sname "p" ]
              )
              (R.CApp
                (R.CVar (R.sname tfun_oname))
                (map (tmplArgToCallCExp b) tfun_args)
              )
        TFunOp {..}    ->
          returnCpp b (tfun_ret) $
            R.CBinOp
              R.CArrow
              (R.CTApp
                 (R.sname "static_cast")
                 [ R.CTStar $ tmplAppTypeFromForm tclass_cxxform typparams ]
                 [ R.CVar $ R.sname "p" ]
              )
              (R.CApp
                (R.CVar (R.sname ("operator" <> opSymbol tfun_opexp)))
                (map (tmplArgToCallCExp b) (argsFromOpExp tfun_opexp))
              )

-- Accessor Declaration and Definition

accessorToDecl :: Variable -> Accessor -> R.CFunDecl Identity
accessorToDecl v a =
  let csig = accessorCFunSig (arg_type (unVariable v)) a
      ret = returnCType (cRetType csig)
      fname =
        R.CName [ R.NamePart "Type"
                , R.NamePart (   "_"
                              <> arg_name (unVariable v)
                              <> "_"
                              <> case a of Getter -> "get"; Setter -> "set"
                             )
                ]
      args = argsToCTypVar (cArgTypes csig)
  in R.CFunDecl ret fname args

accessorsToDecls :: [Variable] -> [R.CFunDecl Identity]
accessorsToDecls vs =
  concatMap (\v -> [accessorToDecl v Getter,accessorToDecl v Setter]) vs

accessorToDef :: Variable -> Accessor -> R.CStatement Identity
accessorToDef v a =
  let varexp =
        R.CBinOp
          R.CArrow
          (R.CTApp
            (R.sname "to_nonconst")
            [ R.CTSimple (R.sname "Type"), R.CTSimple (R.CName [ R.NamePart "Type", R.NamePart "_t"]) ]
            [ R.CVar (R.sname "p") ]
          )
          (R.CVar (R.sname (arg_name (unVariable v))))
      body Getter = R.CReturn $ cxx2C (arg_type (unVariable v)) varexp
      body Setter = R.CExpSA $
                      R.CBinOp
                        R.CAssign
                        varexp
                        (c2Cxx (arg_type (unVariable v)) (R.CVar (R.sname "x")))
  in R.CDefinition Nothing (accessorToDecl v a) [ body a ]


-- Template Member Function Declaration and Definition

-- TODO: Handle simple type
tmplMemberFunToDecl :: Class -> TemplateMemberFunction -> R.CFunDecl Identity
tmplMemberFunToDecl c f =
  let nsuffix = intersperse (R.NamePart "_") $ map R.NamePart (tmf_params f)
      ret = tmplMemFuncReturnCType c (tmf_ret f)
      fname =
        R.CName (R.NamePart (hsTemplateMemberFunctionName c f <> "_") : nsuffix)
      args = map (tmplMemFuncArgToCTypVar c) ((Arg SelfType "p"):tmf_args f)
  in R.CFunDecl ret fname args

-- TODO: Handle simple type
tmplMemberFunToDef :: Class -> TemplateMemberFunction -> R.CStatement Identity
tmplMemberFunToDef c f =
    R.CDefinition (Just R.Inline) (tmplMemberFunToDecl c f) body
  where
    tparams = map (R.CTSimple . R.sname) (tmf_params f)
    body = returnCpp NonCPrim (tmf_ret f) $
             R.CBinOp
               R.CArrow
               (R.CTApp
                 (R.sname "to_nonconst")
                 [ R.CTSimple (R.sname (ffiClassName c)), R.CTSimple (R.sname (ffiClassName c <> "_t")) ]
                 [ R.CVar $ R.sname "p" ]
               )
               (R.CTApp
                 (R.sname (tmf_name f))
                 tparams
                 (map (tmplArgToCallCExp NonCPrim) (tmf_args f))
               )
