{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.ContentMaker
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.ContentMaker where

import           Control.Lens                           ((&),(.~),at)
import           Control.Monad.Trans.Reader
import           Data.Char                              (toUpper)
import           Data.Either                            (rights)
import           Data.Function                          (on)
import qualified Data.Map                          as M
import           Data.Monoid                            ((<>))
import           Data.List                              (intercalate,nub,nubBy)
import           Data.List.Split                        (splitOn)
import           Data.Maybe
import           Data.Text                              (Text)
import           Language.Haskell.Exts.Syntax           (Module(..),Decl(..))
import           System.FilePath
--
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Code.HsFFI             (genHsFFI
                                                        ,genImportInFFI
                                                        ,genTopLevelFuncFFI)
import           FFICXX.Generate.Code.HsFrontEnd
import           FFICXX.Generate.Code.Primitive
import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface  (ClassName(..),HeaderName(..)
                                                        ,Namespace(..)
                                                        ,PackageInterface,PackageName(..)
                                                        ,TypeMacro(..))
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts
--


srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src"

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc"

---- common function for daughter

-- |
mkGlobal :: [Class] -> ClassGlobal
mkGlobal = ClassGlobal <$> mkDaughterSelfMap <*> mkDaughterMap


-- |
buildDaughterDef :: ((String,[Class]) -> String)
              -> DaughterMap
              -> String
buildDaughterDef f m =
    let lst = M.toList m
        f' (x,xs) =  f (x,filter (not.isAbstractClass) xs)
    in (concatMap f' lst)

-- |
buildParentDef :: ((Class,Class)->String) -> Class -> String
buildParentDef f cls = g (class_allparents cls,cls)
  where g (ps,c) = concatMap (\p -> f (p,c)) ps

-- |
mkProtectedFunctionList :: Class -> String
mkProtectedFunctionList c =
    (unlines
     . map (\x->"#define IS_" <> class_name c <> "_" <> x <> "_PROTECTED ()")
     . unProtected . class_protected) c

-- |
buildTypeDeclHeader :: TypeMacro -- ^ typemacro
                 -> [Class]
                 -> String
buildTypeDeclHeader (TypMcro typemacro) classes =
  let typeDeclBodyStr   = genAllCppHeaderMacroType classes
  in subst
       "#ifdef __cplusplus\n\
       \extern \"C\" { \n\
       \#endif\n\
       \\n\
       \#ifndef $typemacro\n\
       \#define $typemacro\n\
       \\n\
       \$typeDeclBody\n\
       \\n\
       \#endif // $typemacro\n\
       \\n\
       \#ifdef __cplusplus\n\
       \}\n\
       \#endif\n"
       (context [ ("typeDeclBody", typeDeclBodyStr )
                , ("typemacro"   , typemacro       ) ])



declarationTemplate :: Text
declarationTemplate =
  "#ifdef __cplusplus\n\
  \extern \"C\" { \n\
  \#endif\n\
  \\n\
  \#ifndef $typemacro\n\
  \#define $typemacro\n\
  \\n\
  \#include \"${cprefix}Type.h\"\n\
  \ // \n\
  \$declarationheader\n\
  \ // \n\
  \$declarationbody\n\
  \\n\
  \#endif // $typemacro\n\
  \\n\
  \#ifdef __cplusplus\n\
  \}\n\
  \#endif\n"

-- |
buildDeclHeader :: TypeMacro  -- ^ typemacro prefix
             -> String     -- ^ C prefix
             -> ClassImportHeader
             -> String
buildDeclHeader (TypMcro typemacroprefix) cprefix header =
  let classes = [cihClass header]
      aclass = cihClass header
      typemacrostr = typemacroprefix <> ffiClassName aclass <> "__"
      declHeaderStr = intercalateWith
                        connRet
                        (\x->"#include \""<>x<>"\"")
                        (map unHdrName (cihIncludedHPkgHeadersInH header))
      declDefStr    = genAllCppHeaderMacroVirtual classes
                      `connRet2`
                      genAllCppHeaderMacroNonVirtual classes
                      `connRet2`
                      genAllCppHeaderMacroAccessor classes
                      `connRet2`
                      genAllCppDefMacroVirtual classes
                      `connRet2`
                      genAllCppDefMacroNonVirtual classes
                      `connRet2`
                      genAllCppDefMacroAccessor classes
      classDeclsStr = -- NOTE: Deletable is treated specially.
                      -- TODO: We had better make it as a separate constructor in Class.
                      if (fst.hsClassName) aclass /= "Deletable"
                        then buildParentDef genCppHeaderInstVirtual aclass
                             `connRet2`
                             genCppHeaderInstVirtual (aclass, aclass)
                             `connRet2`
                             intercalateWith connRet genCppHeaderInstNonVirtual classes
                             `connRet2`
                             intercalateWith connRet genCppHeaderInstAccessor classes
                        else ""
      declBodyStr   = declDefStr
                      `connRet2`
                      classDeclsStr
  in subst declarationTemplate
       (context [ ("typemacro"        , typemacrostr  )
                , ("cprefix"          , cprefix       )
                , ("declarationheader", declHeaderStr )
                , ("declarationbody"  , declBodyStr   ) ])


definitionTemplate :: Text
definitionTemplate =
  "#include<MacroPatternMatch.h>\n\
  \$header\n\
  \\n\
  \$namespace\n\
  \\n\
  \$alias\n\
  \\n\
  \$cppbody\n"


-- |
buildDefMain :: ClassImportHeader
          -> String
buildDefMain cih =
  let classes = [cihClass cih]
      headerStr = genAllCppHeaderInclude cih <> "\n#include \"" <> (unHdrName (cihSelfHeader cih)) <> "\""
      namespaceStr = (concatMap (\x->"using namespace " <> unNamespace x <> ";\n") . cihNamespace) cih
      aclass = cihClass cih
      aliasStr = intercalate "\n" $
                   mapMaybe typedefstmt $
                     aclass : rights (cihImportedClasses cih)
        where typedefstmt c = let n1 = class_name c
                                  n2 = ffiClassName c
                              in if n1 == n2
                                 then Nothing
                                 else Just ("typedef " <> n1 <> " " <> n2 <> ";")

      cppBody = mkProtectedFunctionList (cihClass cih)
                `connRet`
                buildParentDef genCppDefInstVirtual (cihClass cih)
                `connRet`
                if isAbstractClass aclass
                  then ""
                  else genCppDefInstVirtual (aclass, aclass)
                `connRet`
                intercalateWith connRet genCppDefInstNonVirtual classes
                `connRet`
                intercalateWith connRet genCppDefInstAccessor classes
  in subst definitionTemplate (context ([ ("header"   , headerStr    )
                                        , ("alias"    , aliasStr     )
                                        , ("namespace", namespaceStr )
                                        , ("cppbody"  , cppBody      ) ]))

-- |
buildTopLevelHeader :: TypeMacro  -- ^ typemacro prefix
                    -> String     -- ^ C prefix
                    -> TopLevelImportHeader
                    -> String
buildTopLevelHeader (TypMcro typemacroprefix) cprefix tih =
  let typemacrostr = typemacroprefix <> "TOPLEVEL" <> "__"
      declHeaderStr = intercalateWith connRet (\x->"#include \""<>x<>"\"") $
                        map unHdrName $
                             map cihSelfHeader (tihClassDep tih)
      declBodyStr    = intercalateWith connRet genTopLevelFuncCppHeader (tihFuncs tih)
  in subst declarationTemplate (context [ ("typemacro"        , typemacrostr  )
                                        , ("cprefix"          , cprefix       )
                                        , ("declarationheader", declHeaderStr )
                                        , ("declarationbody"  , declBodyStr   ) ])

-- |
buildTopLevelCppDef :: TopLevelImportHeader -> String
buildTopLevelCppDef tih =
  let cihs = tihClassDep tih
      declHeaderStr = "#include \"" <> tihHeaderFileName tih <.> "h" <> "\""
                      `connRet2`
                      (intercalate "\n" (nub (map genAllCppHeaderInclude cihs)))
                      `connRet2`
                      otherHeaders
      otherHeaders =
        intercalateWith connRet (\x->"#include \""<>x<>"\"") $
          map unHdrName $
            map cihSelfHeader cihs ++ tihExtraHeaders tih

      allns = nubBy ((==) `on` unNamespace) ((tihClassDep tih >>= cihNamespace) ++ tihNamespaces tih)

      namespaceStr = do ns <- allns
                        ("using namespace " <> unNamespace ns <> ";\n")
      aliasStr = ""
      declBodyStr    = intercalateWith connRet genTopLevelFuncCppDefinition (tihFuncs tih)

  in subst definitionTemplate (context [ ("header"   , declHeaderStr)
                                       , ("namespace", namespaceStr )
                                       , ("alias"    , aliasStr     )
                                       , ("cppbody"  , declBodyStr  ) ])

-- |
buildTemplateHeader :: TypeMacro  -- ^ typemacro prefix
                    -> TemplateClass
                    -> String
buildTemplateHeader (TypMcro typemacroprefix) t =
  let typemacrostr = typemacroprefix <> "TEMPLATE__" <> map toUpper (tclass_name t) <> "__"
      fs = tclass_funcs t
      deffunc = intercalateWith connRet (genTmplFunCpp False t) fs
                ++ "\n\n"
                ++ intercalateWith connRet (genTmplFunCpp True t) fs
      classlevel = genTmplClassCpp False t fs ++ "\n\n" ++ genTmplClassCpp True t fs
  in subst
       "#ifndef $typemacro\n\
       \#define $typemacro\n\
       \\n\
       \$deffunc\n\
       \$classlevel\n\
       \#endif\n"
       (context [ ("typemacro", typemacrostr )
                , ("deffunc"  , deffunc      )
                , ("classlevel" , classlevel )
                ])


-- |
buildFFIHsc :: ClassModule -> Module ()
buildFFIHsc m = mkModule (mname <.> "FFI") [lang ["ForeignFunctionInterface"]] ffiImports hscBody
  where mname = cmModule m
        headers = cmCIH m
        ffiImports = [ mkImport "Foreign.C", mkImport "Foreign.Ptr", mkImport (mname <.> "RawType") ]
                     <> genImportInFFI m
                     <> genExtraImport m
        hscBody = concatMap genHsFFI headers


-- |
buildRawTypeHs :: ClassModule -> Module ()
buildRawTypeHs m = mkModule (cmModule m <.> "RawType")
                  [lang [ "ForeignFunctionInterface", "TypeFamilies", "MultiParamTypeClasses"
                        , "FlexibleInstances", "TypeSynonymInstances"
                        , "EmptyDataDecls", "ExistentialQuantification", "ScopedTypeVariables" ]]
                  rawtypeImports rawtypeBody
  where rawtypeImports = [ mkImport "Foreign.Ptr"
                         , mkImport "FFICXX.Runtime.Cast"
                         ]
        rawtypeBody = concatMap hsClassRawType . filter (not.isAbstractClass) . cmClass $ m

-- |
buildInterfaceHs :: AnnotateMap -> ClassModule -> Module ()
buildInterfaceHs amap m = mkModule (cmModule m <.> "Interface")
                            [lang [ "EmptyDataDecls", "ExistentialQuantification"
                                  , "FlexibleContexts", "FlexibleInstances", "ForeignFunctionInterface"
                                  , "MultiParamTypeClasses"
                                  , "ScopedTypeVariables"
                                  , "TypeFamilies", "TypeSynonymInstances"
                                  ]]
                            ifaceImports ifaceBody
  where classes = cmClass m
        ifaceImports = [ mkImport "Data.Word"
                       , mkImport "Foreign.C"
                       , mkImport "Foreign.Ptr"
                       , mkImport "FFICXX.Runtime.Cast" ]
                       <> genImportInInterface m
                       <> genExtraImport m
        ifaceBody =
          runReader (mapM genHsFrontDecl classes) amap
          <> (concatMap genHsFrontUpcastClass . filter (not.isAbstractClass)) classes
          <> (concatMap genHsFrontDowncastClass . filter (not.isAbstractClass)) classes

-- |
buildCastHs :: ClassModule -> Module ()
buildCastHs m = mkModule (cmModule m <.> "Cast")
               [ lang [ "FlexibleInstances", "FlexibleContexts", "TypeFamilies"
                      , "MultiParamTypeClasses", "OverlappingInstances", "IncoherentInstances" ] ]
               castImports body
  where classes = cmClass m
        castImports =    [ mkImport "Foreign.Ptr"
                         , mkImport "FFICXX.Runtime.Cast"
                         , mkImport "System.IO.Unsafe" ]
                      <> genImportInCast m
        body =    mapMaybe genHsFrontInstCastable classes
               <> mapMaybe genHsFrontInstCastableSelf classes

-- |
buildImplementationHs :: AnnotateMap -> ClassModule -> Module ()
buildImplementationHs amap m = mkModule (cmModule m <.> "Implementation")
                                 [ lang [ "EmptyDataDecls"
                                        , "FlexibleContexts", "FlexibleInstances", "ForeignFunctionInterface"
                                        , "IncoherentInstances"
                                        , "MultiParamTypeClasses"
                                        , "OverlappingInstances"
                                        , "TypeFamilies", "TypeSynonymInstances"
                                        ] ]
                                 implImports implBody
  where classes = cmClass m
        implImports = [ mkImport "FFICXX.Runtime.Cast"
                      , mkImport "Data.Word"
                      , mkImport "Foreign.C"
                      , mkImport "Foreign.Ptr"
                      , mkImport "System.IO.Unsafe" ]
                      <> genImportInImplementation m
                      <> genExtraImport m
        f :: Class -> [Decl ()]
        f y = concatMap (flip genHsFrontInst y) (y:class_allparents y)

        implBody =    concatMap f classes
                   <> runReader (concat <$> mapM genHsFrontInstNew classes) amap
                   <> concatMap genHsFrontInstNonVirtual classes
                   <> concatMap genHsFrontInstStatic classes
                   <> concatMap genHsFrontInstVariables classes

buildTemplateHs :: TemplateClassModule -> Module ()
buildTemplateHs m = mkModule (tcmModule m <.> "Template")
                   [lang  [ "EmptyDataDecls", "FlexibleInstances", "MultiParamTypeClasses"
                          , "TypeFamilies"] ]
                   [ mkImport "Foreign.C.Types"
                   , mkImport "Foreign.Ptr"
                   , mkImport "FFICXX.Runtime.Cast"
                   ]
                   body
  where ts = tcmTemplateClasses m
        body = concatMap genTmplInterface ts

buildTHHs :: TemplateClassModule -> Module ()
buildTHHs m = mkModule (tcmModule m <.> "TH")
             [lang  ["TemplateHaskell"] ]
             ([ mkImport "Data.Char"
              , mkImport "Data.Monoid"
              , mkImport "Foreign.C.Types"
              , mkImport "Foreign.Ptr"
              , mkImport "Language.Haskell.TH"
              , mkImport "Language.Haskell.TH.Syntax"
              , mkImport "FFICXX.Runtime.TH"
              ] <> imports)
             body
  where ts = tcmTemplateClasses m
        imports = [ mkImport (tcmModule m <.> "Template") ]
        body = concatMap genTmplImplementation ts
               <> concatMap (\t -> genTmplInstance t (tclass_funcs t)) ts

-- |
buildInterfaceHSBOOT :: String -> Module ()
buildInterfaceHSBOOT mname = mkModule (mname <.> "Interface") [] [] hsbootBody
  where cname = last (splitOn "." mname)
        hsbootBody = [ mkClass cxEmpty ('I':cname) [mkTBind "a"] [] ]

-- |
buildModuleHs :: ClassModule -> Module ()
buildModuleHs m = mkModuleE (cmModule m) [] (concatMap genExport (cmClass m)) (genImportInModule (cmClass m)) []

-- |
buildTopLevelHs :: String -> ([ClassModule],[TemplateClassModule]) -> TopLevelImportHeader -> Module ()
buildTopLevelHs modname (mods,tmods) tih =
    mkModuleE modname pkgExtensions pkgExports pkgImports pkgBody
  where
    tfns = tihFuncs tih
    pkgExtensions = [ lang [ "FlexibleContexts", "FlexibleInstances" ] ]
    pkgExports =     map (emodule . cmModule) mods
                 ++  map (evar . unqual . hsFrontNameForTopLevelFunction) tfns

    pkgImports = genImportInTopLevel modname (mods,tmods) tih

    pkgBody    =    map (genTopLevelFuncFFI tih) tfns
                 ++ concatMap genTopLevelFuncDef tfns

-- |
buildPackageInterface :: PackageInterface
                      -> PackageName
                      -> [ClassImportHeader]
                      -> PackageInterface
buildPackageInterface pinfc pkgname = foldr f pinfc
  where f cih repo =
          let name = (class_name . cihClass) cih
              header = cihSelfHeader cih
          in repo & at (pkgname,ClsName name) .~ (Just header)
