{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Cpp
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
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
import           FFICXX.Generate.Util
import           FFICXX.Generate.Code.MethodDef
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface
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
                      in subst tmpl (context [ ("classname", class_name c) ])

genAllCppHeaderTmplType :: [Class] -> String
genAllCppHeaderTmplType = intercalateWith connRet2 (genCppHeaderTmplType) 

---- "Class Declaration Virtual" Declaration 

genCppHeaderTmplVirtual :: Class -> String 
genCppHeaderTmplVirtual aclass =  
  let tmpl = "#undef ${classname}_DECL_VIRT \n#define ${classname}_DECL_VIRT(Type) \\\n${funcdecl}"
      funcDeclStr = (funcsToDecls aclass) . virtualFuncs . class_funcs $ aclass
  in subst tmpl (context [ ("classname", map toUpper (class_name aclass) ) 
                         , ("funcdecl" , funcDeclStr                     ) ]) 
      
genAllCppHeaderTmplVirtual :: [Class] -> String 
genAllCppHeaderTmplVirtual = intercalateWith connRet2 genCppHeaderTmplVirtual

---- "Class Declaration Non-Virtual" Declaration

genCppHeaderTmplNonVirtual :: Class -> String
genCppHeaderTmplNonVirtual c = 
  let tmpl = "#undef ${classname}_DECL_NONVIRT \n#define ${classname}_DECL_NONVIRT(Type) \\\n$funcdecl" 
      declBodyStr = subst tmpl (context [ ("classname", map toUpper (class_name c))
                                        , ("funcdecl" , funcDeclStr               ) ])
      funcDeclStr = (funcsToDecls c) . filter (not.isVirtualFunc) 
                                     . class_funcs $ c
  in  declBodyStr 

genAllCppHeaderTmplNonVirtual :: [Class] -> String 
genAllCppHeaderTmplNonVirtual = intercalateWith connRet genCppHeaderTmplNonVirtual

---- "Class Declaration Virtual/NonVirtual" Instances

genCppHeaderInstVirtual :: (Class,Class) -> String 
genCppHeaderInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  strc<>"_DECL_VIRT(" <> class_name c <> ");\n"

genCppHeaderInstNonVirtual :: Class -> String 
genCppHeaderInstNonVirtual c = 
  let strx = map toUpper (class_name c) 
  in  strx<>"_DECL_NONVIRT(" <> class_name c <> ");\n" 

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
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (class_name aclass) ) 
                                       , ("funcdef"  , funcDefStr                      ) ]) 
      funcDefStr = (funcsToDefs aclass) . virtualFuncs . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplVirtual :: [Class] -> String
genAllCppDefTmplVirtual = intercalateWith connRet2 genCppDefTmplVirtual

---- "Class Definition NonVirtual" Declaration

genCppDefTmplNonVirtual :: Class -> String 
genCppDefTmplNonVirtual aclass =  
  let tmpl = "#undef ${classname}_DEF_NONVIRT\n#define ${classname}_DEF_NONVIRT(Type)\\\n$funcdef" 
      defBodyStr = subst tmpl (context [ ("classname", map toUpper (class_name aclass) ) 
                                       , ("funcdef"  , funcDefStr                      ) ]) 
      funcDefStr = (funcsToDefs aclass) . filter (not.isVirtualFunc) 
                                        . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplNonVirtual :: [Class] -> String
genAllCppDefTmplNonVirtual = intercalateWith connRet2 genCppDefTmplNonVirtual

---- "Class Definition Virtual/NonVirtual" Instances

genCppDefInstVirtual :: (Class,Class) -> String 
genCppDefInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  strc<>"_DEF_VIRT(" <> class_name c <> ")\n"

genCppDefInstNonVirtual :: Class -> String
genCppDefInstNonVirtual c = 
  subst "${capitalclassname}_DEF_NONVIRT(${classname})"
    (context [ ("capitalclassname", toUppers (class_name c))
             , ("classname"       , class_name c           ) ]) 

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

{-
        case toplevelfunc_ret of          
        Void                    -> callstr <> ";"
        SelfType                -> "return to_nonconst<Type ## _t, Type>((Type *)" <> callstr <> ") ;"
        CT (CRef _) _           -> "return ((*)"<>callstr<>");"
        CT _ _                  -> "return "<>callstr<>";" 
        CPT (CPTClass c') _     -> "return to_nonconst<"<>str<>"_t,"<>str
                                    <>">(("<>str<>"*)"<>callstr<>");" 
                                    where str = class_name c' 
        CPT (CPTClassRef _c') _ -> "return ((*)"<>callstr<>");"
        CPT (CPTClassCopy c') _ -> "return (new "<>str<>"("<>callstr<>"));"
                                    where str = class_name c'
        
        TemplateApp _ _ _       -> error "genTopLevelFuncCppDefinition: TemplateApp"
        TemplateType _          -> error "genTopLevelFuncCppDefinition: TemplateType"
        TemplateParam _         -> error "genTopLevelFuncCppDefinition: TemplateParam"   
      funcDefStr = returnstr  -}
  in subst tmpl (context [ ("returntype", rettypeToString toplevelfunc_ret                )  
                         , ("funcname"  , "TopLevel_" 
                                          <> maybe toplevelfunc_name id toplevelfunc_alias)
                         , ("args"      , argsToStringNoSelf toplevelfunc_args            ) 
                         , ("funcbody"  , funcDefStr                                      ) ])
genTopLevelFuncCppDefinition TopLevelVariable {..} =  
  let tmpl = "$returntype $funcname ( ) { \n  $funcbody\n}" 
      callstr = toplevelvar_name
      funcDefStr = returnCpp False (toplevelvar_ret) callstr
      {- 
        Void -> callstr <> ";"
        SelfType                -> "return to_nonconst<Type ## _t, Type>((Type *)" <> callstr <> ") ;"
        CT (CRef _) _           -> "return ((*)"<>callstr<>");"
        CT _ _                  -> "return "<>callstr<>";" 
        CPT (CPTClass c') _     -> "return to_nonconst<"<>str<>"_t,"<>str
                                   <>">(("<>str<>"*)"<>callstr<>");" 
                                   where str = class_name c' 
        CPT (CPTClassRef _c') _ -> "return ((*)"<>callstr<>");"
        CPT (CPTClassCopy c') _ -> "return (new "<>str<>"("<>callstr<>"));"
                                    where str = class_name c'
        
        TemplateApp _ _ _       -> error "genTopLevelFuncCppDefinition: TemplateApp"
        TemplateType _          -> error "genTopLevelFuncCppDefinition: TemplateType"
        TemplateParam _         -> error "genTopLevelFuncCppDefinition: TemplateParam"   
      funcDefStr = returnstr  -}
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
                 
