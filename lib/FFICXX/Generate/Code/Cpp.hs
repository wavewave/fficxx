-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Cpp
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------


module FFICXX.Generate.Code.Cpp where

import Data.Char 
import Data.List
import System.FilePath

import FFICXX.Generate.Util
-- import FFICXX.Generate.Type.Method
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Code.MethodDef
import FFICXX.Generate.Code.Cabal

-- Class Declaration and Definition

----
---- Declaration
----

---- "Class Type Declaration" Instances

genCppHeaderTmplType :: Class -> String 
genCppHeaderTmplType c = let tmpl = "ROOT_TYPE_DECLARATION($classname$);" 
                     in  render tmpl [ ("classname", class_name c) ] 

genAllCppHeaderTmplType :: [Class] -> String
genAllCppHeaderTmplType = intercalateWith connRet (genCppHeaderTmplType) 

---- "Class Declaration Virtual" Declaration 

genCppHeaderTmplVirtual :: Class -> String 
genCppHeaderTmplVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DECLARATIONVIRT\\\n#define ROOT_$classname$_DECLARATIONVIRT(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = (funcsToDecls aclass) . virtualFuncs . class_funcs $ aclass
  in  declBodyStr 
      
genAllCppHeaderTmplVirtual :: [Class] -> String 
genAllCppHeaderTmplVirtual = intercalateWith connRet2 genCppHeaderTmplVirtual

---- "Class Declaration Non-Virtual" Declaration

genCppHeaderTmplNonVirtual :: Class -> String
genCppHeaderTmplNonVirtual c = 
  let tmpl = "#undef ROOT_$classname$_DECLARATIONNONVIRT\\\n#define ROOT_$classname$_DECLARATIONNONVIRT(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name c) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = (funcsToDecls c) . filter (not.isVirtualFunc) 
                                     . class_funcs $ c
  in  declBodyStr 

genAllCppHeaderTmplNonVirtual :: [Class] -> String 
genAllCppHeaderTmplNonVirtual = intercalateWith connRet genCppHeaderTmplNonVirtual

---- "Class Declaration Virtual/NonVirtual" Instances

genCppHeaderInstVirtual :: (Class,Class) -> String 
genCppHeaderInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  "ROOT_"++strc++"_DECLARATIONVIRT(" ++ class_name c ++ ");\n"

genCppHeaderInstNonVirtual :: Class -> String 
genCppHeaderInstNonVirtual c = 
  let strx = map toUpper (class_name c) 
  in  "ROOT_"++strx++"_DECLARATIONNONVIRT(" ++ class_name c ++ ");\n" 

genAllCppHeaderInstNonVirtual :: [Class] -> String 
genAllCppHeaderInstNonVirtual = 
  intercalateWith connRet genCppHeaderInstNonVirtual


----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefTmplVirtual :: Class -> String 
genCppDefTmplVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITIONVIRT\\\n#define ROOT_$classname$_DEFINITIONVIRT(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = (funcsToDefs aclass) . virtualFuncs . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplVirtual :: [Class] -> String
genAllCppDefTmplVirtual = intercalateWith connRet2 genCppDefTmplVirtual

---- "Class Definition NonVirtual" Declaration

genCppDefTmplNonVirtual :: Class -> String 
genCppDefTmplNonVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITIONNONVIRT\\\n#define ROOT_$classname$_DEFINITIONNONVIRT(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = (funcsToDefs aclass) . filter (not.isVirtualFunc) 
                                        . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplNonVirtual :: [Class] -> String
genAllCppDefTmplNonVirtual = intercalateWith connRet2 genCppDefTmplNonVirtual

---- "Class Definition Virtual/NonVirtual" Instances

genCppDefInstVirtual :: (Class,Class) -> String 
genCppDefInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  "ROOT_"++strc++"_DEFINITIONVIRT(" ++ class_name c ++ ")\n"

genCppDefInstNonVirtual :: Class -> String
genCppDefInstNonVirtual c = 
  let tmpl = "ROOT_$capitalclassname$_DEFINITIONNONVIRT($classname$)" 
  in  render tmpl [ ("capitalclassname", toUppers (class_name c))
                  , ("classname", class_name c) ] 

genAllCppDefInstNonVirtual :: [Class] -> String 
genAllCppDefInstNonVirtual = 
  intercalateWith connRet genCppDefInstNonVirtual

-----


-----------------

genAllCppHeaderInclude :: ClassImportHeader -> String 
genAllCppHeaderInclude header = 
    intercalateWith connRet (\x->"#include \""++x++"\"") $
      cihIncludedHPkgHeadersInCPP header
        ++ cihIncludedCPkgHeaders header

genModuleIncludeHeader :: [ClassImportHeader] -> String 
genModuleIncludeHeader headers =
  let strlst = map ((\x->"#include \""++x++"\"") . cihSelfHeader) headers 
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
      includeFileStrs = map (\x->indent++x) selfheaders
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
      includeFileStrsWithCsrc = map (\x->indent++"csrc"</>x) 
                                 (if (null.tihFuncs) tih then selfheaders else tlh:selfheaders)
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
genTopLevelFuncCppHeader f =  
    let tmpl = "$returntype$ $funcname$ ( $args$ );" 
    in  render tmpl [ ("returntype", rettypeToString (toplevelfunc_ret f))  
                    , ("funcname", "TopLevel_" ++ maybe (toplevelfunc_name f) id (toplevelfunc_alias f))
                    , ("args", argsToStringNoSelf (toplevelfunc_args f)) ] 

genTopLevelFuncCppDefinition :: TopLevelFunction -> String 
genTopLevelFuncCppDefinition f =  
    let tmpl = "$returntype$ $funcname$ ( $args$ ) { \\\n  $funcbody$\\\n}" 
        callstr = toplevelfunc_name f ++ "("
                  ++ argsToCallString (toplevelfunc_args f)   
                  ++ ")"
        returnstr = case toplevelfunc_ret f of          
          Void -> callstr ++ ";"
          SelfType -> "return to_nonconst<Type ## _t, Type>((Type *)" ++ callstr ++ ") ;"
          (CT _ctyp _isconst) -> "return "++callstr++";" 
          (CPT (CPTClass c') _) -> "return to_nonconst<"++str++"_t,"++str
                                    ++">(("++str++"*)"++callstr++");" 
            where str = class_name c' 
          (CPT (CPTClassRef _c') _) -> "return ((*)"++callstr++");" 
        funcDefStr = returnstr 
    in  render tmpl [ ("returntype", rettypeToString (toplevelfunc_ret f))  
                    , ("funcname", "TopLevel_" ++ maybe (toplevelfunc_name f) id (toplevelfunc_alias f))
                    , ("args", argsToStringNoSelf (toplevelfunc_args f)) 
                    , ("funcbody", funcDefStr )
                    ] 


