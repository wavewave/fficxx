{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Cpp
-- Copyright   : (c) 2011-2013,2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.Cpp where

import           Data.Char 
import           Data.List
import           Data.Text                              (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Text.Template                     hiding (render)
import           System.FilePath

import           FFICXX.Generate.Util
import           FFICXX.Generate.Code.MethodDef
import           FFICXX.Generate.Code.Cabal
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.PackageInterface

-- Class Declaration and Definition

----
---- Declaration
----

---- "Class Type Declaration" Instances

genCppHeaderTmplType :: Class -> String 
genCppHeaderTmplType c = let tmpl = "// Opaque type definition for $classname \n\
                                    \typedef struct ${classname}_tag ${classname}_t; \n\
                                    \typedef ${classname}_t * ${classname}_p; \n\
                                    \typedef ${classname}_t const* const_{classname}_p; \n"
                      in TL.unpack $ substitute tmpl (context [ ("classname", class_name c) ])

genAllCppHeaderTmplType :: [Class] -> String
genAllCppHeaderTmplType = intercalateWith connRet2 (genCppHeaderTmplType) 

---- "Class Declaration Virtual" Declaration 

genCppHeaderTmplVirtual :: Class -> String 
genCppHeaderTmplVirtual aclass =  
  let tmpl = "#undef ${classname}_DECL_VIRT\\\n#define ${classname}_DECL_VIRT(Type) \\\\\\\n$funcdecl" 
      declBodyStr = TL.unpack $ substitute tmpl
                                  (context [ ("classname", map toUpper (class_name aclass) ) 
                                           , ("funcdecl" , funcDeclStr )
                                           ]) 
      funcDeclStr = (funcsToDecls aclass) . virtualFuncs . class_funcs $ aclass
  in  declBodyStr 
      
genAllCppHeaderTmplVirtual :: [Class] -> String 
genAllCppHeaderTmplVirtual = intercalateWith connRet2 genCppHeaderTmplVirtual

---- "Class Declaration Non-Virtual" Declaration

genCppHeaderTmplNonVirtual :: Class -> String
genCppHeaderTmplNonVirtual c = 
  let tmpl = "#undef ${classname}_DECL_NONVIRT\\\n#define ${classname}_DECL_NONVIRT(Type) \\\\\\\n$funcdecl" 
      declBodyStr = TL.unpack $ substitute tmpl
                                  (context [ ("classname", map toUpper (class_name c))
                                           , ("funcdecl" , funcDeclStr )               ])
      funcDeclStr = (funcsToDecls c) . filter (not.isVirtualFunc) 
                                     . class_funcs $ c
  in  declBodyStr 

genAllCppHeaderTmplNonVirtual :: [Class] -> String 
genAllCppHeaderTmplNonVirtual = intercalateWith connRet genCppHeaderTmplNonVirtual

---- "Class Declaration Virtual/NonVirtual" Instances

genCppHeaderInstVirtual :: (Class,Class) -> String 
genCppHeaderInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  strc++"_DECL_VIRT(" ++ class_name c ++ ");\n"

genCppHeaderInstNonVirtual :: Class -> String 
genCppHeaderInstNonVirtual c = 
  let strx = map toUpper (class_name c) 
  in  strx++"_DECL_NONVIRT(" ++ class_name c ++ ");\n" 

genAllCppHeaderInstNonVirtual :: [Class] -> String 
genAllCppHeaderInstNonVirtual = 
  intercalateWith connRet genCppHeaderInstNonVirtual


----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefTmplVirtual :: Class -> String 
genCppDefTmplVirtual aclass =  
  let tmpl = "#undef ${classname}_DEF_VIRT\\\n#define ${classname}_DEF_VIRT(Type)\\\\\\\n$funcdef" 
      defBodyStr = TL.unpack $ substitute tmpl
                                 (context [ ("classname", map toUpper (class_name aclass) ) 
                                          , ("funcdef" , funcDefStr )                       ]) 
      funcDefStr = (funcsToDefs aclass) . virtualFuncs . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplVirtual :: [Class] -> String
genAllCppDefTmplVirtual = intercalateWith connRet2 genCppDefTmplVirtual

---- "Class Definition NonVirtual" Declaration

genCppDefTmplNonVirtual :: Class -> String 
genCppDefTmplNonVirtual aclass =  
  let tmpl = "#undef ${classname}_DEF_NONVIRT\\\n#define ${classname}_DEF_NONVIRT(Type)\\\\\\\n$funcdef" 
      defBodyStr = TL.unpack $ substitute tmpl
                                 (context [ ("classname", map toUpper (class_name aclass) ) 
                                          , ("funcdef" , funcDefStr )                       ]) 
      funcDefStr = (funcsToDefs aclass) . filter (not.isVirtualFunc) 
                                        . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplNonVirtual :: [Class] -> String
genAllCppDefTmplNonVirtual = intercalateWith connRet2 genCppDefTmplNonVirtual

---- "Class Definition Virtual/NonVirtual" Instances

genCppDefInstVirtual :: (Class,Class) -> String 
genCppDefInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  strc++"_DEF_VIRT(" ++ class_name c ++ ")\n"

genCppDefInstNonVirtual :: Class -> String
genCppDefInstNonVirtual c = 
  let tmpl = "${capitalclassname}_DEF_NONVIRT(${classname})" 
  in TL.unpack $ substitute tmpl
                   (context [ ("capitalclassname", toUppers (class_name c))
                            , ("classname", class_name c)                   ]) 

genAllCppDefInstNonVirtual :: [Class] -> String 
genAllCppDefInstNonVirtual = 
  intercalateWith connRet genCppDefInstNonVirtual

-----------------

genAllCppHeaderInclude :: ClassImportHeader -> String 
genAllCppHeaderInclude header = 
    intercalateWith connRet (\x->"#include \""++x++"\"") $
      map unHdrName (cihIncludedHPkgHeadersInCPP header
                     ++ cihIncludedCPkgHeaders header)


genModuleIncludeHeader :: [ClassImportHeader] -> String 
genModuleIncludeHeader headers =
  let strlst = map ((\x->"#include \""++x++"\"") . unHdrName . cihSelfHeader) headers 
  in  intercalate "\n" strlst 

----

genIncludeFiles :: String        -- ^ package name 
                -> [ClassModule] 
                -> String
genIncludeFiles pkgname cmods =
  let indent = cabalIndentation 
      selfheaders' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfHeader y) 
      selfheaders = nub selfheaders'
      includeFileStrs = map ((indent++).unHdrName) selfheaders
  in  unlines ((indent++pkgname++"Type.h") : includeFileStrs)

genCsrcFiles :: (TopLevelImportHeader,[ClassModule]) -> String
genCsrcFiles (tih,cmods) =
  let indent = cabalIndentation 
      selfheaders' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfHeader y) 
      selfheaders = nub selfheaders'
      selfcpp' = do 
        x <- cmods
        y <- cmCIH x 
        return (cihSelfCpp y)
      selfcpp = nub selfcpp' 
      tlh = tihHeaderFileName tih <.> "h"
      tlcpp = tihHeaderFileName tih <.> "cpp"
      includeFileStrsWithCsrc = map (\x->indent++"csrc"</> x) 
                                 (if (null.tihFuncs) tih then map unHdrName selfheaders else tlh:(map unHdrName selfheaders))
      cppFilesWithCsrc = map (\x->indent++"csrc"</>x) 
                           (if (null.tihFuncs) tih then selfcpp else tlcpp:selfcpp)
      
  in  unlines (includeFileStrsWithCsrc ++ cppFilesWithCsrc)

genCppFiles :: (TopLevelImportHeader,[ClassModule]) -> String 
genCppFiles (tih,cmods) = 
  let indent = cabalIndentation 
      selfcpp' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfCpp y) 
      selfcpp = nub selfcpp'
      tlcpp = tihHeaderFileName tih <.> "cpp"
      cppFileStrs = map (\x->indent++ "csrc" </> x) 
                      (if (null.tihFuncs) tih then selfcpp else tlcpp:selfcpp)
  in  unlines cppFileStrs 



-------------------------
-- TOP LEVEL FUNCTIONS --
-------------------------

genTopLevelFuncCppHeader :: TopLevelFunction -> String 
genTopLevelFuncCppHeader TopLevelFunction {..} = 
    let tmpl = "$returntype $funcname ( $args );" 
    in TL.unpack $ substitute tmpl
                     (context [ ("returntype", rettypeToString toplevelfunc_ret)  
                              , ("funcname", "TopLevel_" 
                                             ++ maybe toplevelfunc_name id toplevelfunc_alias)
                              , ("args", argsToStringNoSelf toplevelfunc_args)
                              ])
genTopLevelFuncCppHeader TopLevelVariable {..} = 
    let tmpl = "$returntype$ $funcname$ ( );" 
    in TL.unpack $ substitute tmpl
                     (context [ ("returntype", rettypeToString toplevelvar_ret)  
                              , ("funcname", "TopLevel_" 
                                             ++ maybe toplevelvar_name id toplevelvar_alias)
                              ]) 

genTopLevelFuncCppDefinition :: TopLevelFunction -> String 
genTopLevelFuncCppDefinition TopLevelFunction {..} =  
    let tmpl = "$returntype $funcname ( $args ) { \\\n  $funcbody\\\n}" 
        callstr = toplevelfunc_name ++ "("
                  ++ argsToCallString toplevelfunc_args   
                  ++ ")"
        returnstr = case toplevelfunc_ret of          
          Void -> callstr ++ ";"
          SelfType -> "return to_nonconst<Type ## _t, Type>((Type *)" ++ callstr ++ ") ;"
          (CT (CRef _) _) -> "return ((*)"++callstr++");"
          (CT _ctyp _isconst) -> "return "++callstr++";" 
          (CPT (CPTClass c') _) -> "return to_nonconst<"++str++"_t,"++str
                                    ++">(("++str++"*)"++callstr++");" 
            where str = class_name c' 
          (CPT (CPTClassRef _c') _) -> "return ((*)"++callstr++");" 
        funcDefStr = returnstr 
    in TL.unpack $ substitute tmpl
                     (context [ ("returntype", rettypeToString toplevelfunc_ret)  
                              , ("funcname", "TopLevel_" 
                                             ++ maybe toplevelfunc_name id toplevelfunc_alias)
                              , ("args", argsToStringNoSelf toplevelfunc_args) 
                              , ("funcbody", funcDefStr )
                              ] )
genTopLevelFuncCppDefinition TopLevelVariable {..} =  
    let tmpl = "$returntype $funcname ( ) { \\\n  $funcbody\\\n}" 
        callstr = toplevelvar_name
        returnstr = case toplevelvar_ret of          
          Void -> callstr ++ ";"
          SelfType -> "return to_nonconst<Type ## _t, Type>((Type *)" ++ callstr ++ ") ;"
          (CT _ctyp _isconst) -> "return "++callstr++";" 
          (CT (CRef _) _) -> "return ((*)"++callstr++");"
          (CPT (CPTClass c') _) -> "return to_nonconst<"++str++"_t,"++str
                                    ++">(("++str++"*)"++callstr++");" 
            where str = class_name c' 
          (CPT (CPTClassRef _c') _) -> "return ((*)"++callstr++");" 
        funcDefStr = returnstr 
    in TL.unpack $ substitute tmpl
                     (context [ ("returntype", rettypeToString toplevelvar_ret)  
                              , ("funcname", "TopLevel_" 
                                             ++ maybe toplevelvar_name id toplevelvar_alias)
                              , ("funcbody", funcDefStr )
                              ])


