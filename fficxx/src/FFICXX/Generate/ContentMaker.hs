{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.ContentMaker where

import Control.Lens                           ((&),(.~),at)
import Control.Monad.Trans.Reader
import Data.Char                              (toUpper)
import Data.Either                            (rights)
import Data.Function                          (on)
import qualified Data.Map as M
import Data.Maybe                             (mapMaybe,maybeToList)
import Data.Monoid                            ((<>))
import Data.List                              (find,intercalate,nub,nubBy)
import Data.List.Split                        ( splitOn )
import Data.Text                              ( Text )
import qualified Data.Text as T               ( pack )
import Language.Haskell.Exts.Syntax           ( Module(..)
                                              , Decl(..)
                                              )
import System.FilePath
--
import FFICXX.Runtime.CodeGen.C               ( CStatement(..)
                                              , HeaderName(..)
                                              , Namespace(..)
                                              , render
                                              )
--
import FFICXX.Generate.Code.Cpp
import FFICXX.Generate.Code.HsCast            (genHsFrontInstCastable
                                              ,genHsFrontInstCastableSelf)
import FFICXX.Generate.Code.HsFFI             (genHsFFI
                                              ,genImportInFFI
                                              ,genTopLevelFuncFFI)
import FFICXX.Generate.Code.HsFrontEnd
import FFICXX.Generate.Code.HsTemplate        (genTemplateMemberFunctions
                                              ,genTmplInstance
                                              ,genTmplInterface
                                              ,genTmplImplementation
                                              )
import FFICXX.Generate.Dependency
import FFICXX.Generate.Name                   (ffiClassName,hsClassName
                                              ,hsFrontNameForTopLevelFunction)
import FFICXX.Generate.Type.Annotate
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface  ( ClassName(..)
                                              , PackageInterface
                                              , PackageName(..)
                                              , TypeMacro(..)
                                              )
import FFICXX.Generate.Util
import FFICXX.Generate.Util.HaskellSrcExts


srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src"

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc"

---- common function for daughter

-- |
mkGlobal :: [Class] -> ClassGlobal
mkGlobal = ClassGlobal <$> mkDaughterSelfMap <*> mkDaughterMap


-- |
buildDaughterDef ::
     ((String,[Class]) -> String)
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
  let typeDeclBodyStr = intercalateWith connRet2 (genCppHeaderMacroType) classes
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
buildDeclHeader ::
     TypeMacro  -- ^ typemacro prefix
  -> String     -- ^ C prefix
  -> ClassImportHeader
  -> String
buildDeclHeader (TypMcro typemacroprefix) cprefix header =
  let classes = [cihClass header]
      aclass = cihClass header
      typemacrostr = typemacroprefix <> ffiClassName aclass <> "__"
      declHeaderStr =
        render (Include (HdrName (cprefix ++ "Type.h")))
        `connRet2`
        intercalate "\n" (map (render . Include) $ cihIncludedHPkgHeadersInH header)
      declDefStr    = intercalateWith connRet2 genCppHeaderMacroVirtual classes
                      `connRet2`
                      intercalateWith connRet genCppHeaderMacroNonVirtual classes
                      `connRet2`
                      intercalateWith connRet genCppHeaderMacroAccessor classes
                      `connRet2`
                      intercalateWith connRet2 genCppDefMacroVirtual classes
                      `connRet2`
                      intercalateWith connRet2 genCppDefMacroNonVirtual classes
                      `connRet2`
                      intercalateWith connRet2 genCppDefMacroAccessor classes
                      `connRet2`
                      flip (intercalateWith connRet2) classes
                        (\c -> intercalateWith connRet2
                                 (genCppDefMacroTemplateMemberFunction c)
                                 (class_tmpl_funcs c)
                        )
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
                , ("declarationheader", declHeaderStr )
                , ("declarationbody"  , declBodyStr   ) ])


definitionTemplate :: Text
definitionTemplate =
  "$header\n\
  \\n\
  \$alias\n\
  \\n\
  \#define CHECKPROTECT(x,y) IS_PAREN(IS_ ## x ## _ ## y ## _PROTECTED)\n\
  \\n\
  \#define TYPECASTMETHOD(cname,mname,oname) \\\n\
  \  IIF( CHECKPROTECT(cname,mname) ) ( \\\n\
  \  (to_nonconst<oname,cname ## _t>), \\\n\
  \  (to_nonconst<cname,cname ## _t>) )\n\
  \\n\
  \$cppbody\n"


-- |
buildDefMain :: ClassImportHeader
          -> String
buildDefMain cih =
  let classes = [cihClass cih]
      headerStr =
           render (Include "MacroPatternMatch.h")
        <> genAllCppHeaderInclude cih
        <> render (Include (cihSelfHeader cih))
        <> "\n"
        <> (concatMap (render . UsingNamespace) . cihNamespace) cih
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
  in subst
       definitionTemplate
       (context ([ ("alias"    , aliasStr     )
                 , ("header", headerStr)
                 , ("cppbody"  , cppBody      ) ]))

-- |
buildTopLevelHeader :: TypeMacro  -- ^ typemacro prefix
                    -> String     -- ^ C prefix
                    -> TopLevelImportHeader
                    -> String
buildTopLevelHeader (TypMcro typemacroprefix) cprefix tih =
  let typemacrostr = typemacroprefix <> "TOPLEVEL" <> "__"
      declHeaderStr =
        render (Include (HdrName (cprefix ++ "Type.h")))
        `connRet2`
        intercalate "\n"
          (map (render . Include) (map cihSelfHeader (tihClassDep tih) ++ tihExtraHeadersInH tih))
      declBodyStr    = intercalateWith connRet genTopLevelFuncCppHeader (tihFuncs tih)
  in subst declarationTemplate (context [ ("typemacro"        , typemacrostr  )
                                        , ("declarationheader", declHeaderStr )
                                        , ("declarationbody"  , declBodyStr   ) ])

-- |
buildTopLevelCppDef :: TopLevelImportHeader -> String
buildTopLevelCppDef tih =
  let cihs = tihClassDep tih
      extclasses = tihExtraClassDep tih
      declHeaderStr =
        render (Include "MacroPatternMatch.h")
        `connRet2`
        render (Include (HdrName (tihHeaderFileName tih <.> "h")))
        `connRet2`
        (intercalate "\n" (nub (map genAllCppHeaderInclude cihs)))
        `connRet2`
        otherHeaders
        `connRet2`
        namespaceStr
      otherHeaders =
        intercalate "\n" $ map (render . Include) $
          map cihSelfHeader cihs
          ++ tihExtraHeadersInCPP tih

      allns = nub ((tihClassDep tih >>= cihNamespace) ++ tihNamespaces tih)

      namespaceStr = do ns <- allns
                        render (UsingNamespace ns)
      aliasStr = intercalate "\n" $
                   mapMaybe typedefstmt $
                     rights (concatMap cihImportedClasses cihs ++ extclasses)
        where typedefstmt c = let n1 = class_name c
                                  n2 = ffiClassName c
                              in if n1 == n2
                                 then Nothing
                                 else Just ("typedef " <> n1 <> " " <> n2 <> ";")
      declBodyStr    = intercalateWith connRet genTopLevelFuncCppDefinition (tihFuncs tih)

  in subst definitionTemplate (context [ ("header"   , declHeaderStr)
                                       , ("alias"    , aliasStr     )
                                       , ("cppbody"  , declBodyStr  ) ])

-- |
buildTemplateHeader :: TypeMacro  -- ^ typemacro prefix
                    -> TemplateClassImportHeader
                    -> String
buildTemplateHeader (TypMcro typemacroprefix) tcih =
  let
      t = tcihTClass tcih
      typemacrostr = typemacroprefix <> "TEMPLATE__" <> map toUpper (tclass_name t) <> "__"
      fs = tclass_funcs t

      headerStr = concatMap (render . Include) (tcihCxxHeaders tcih)

      deffunc = intercalateWith connRet (genTmplFunCpp False t) fs
                ++ "\n\n"
                ++ intercalateWith connRet (genTmplFunCpp True t) fs
      classlevel = genTmplClassCpp False t fs ++ "\n\n" ++ genTmplClassCpp True t fs
  in subst
       "#ifndef $typemacro\n\
       \#define $typemacro\n\
       \\n\
       \$headers\n\
       \\n\
       \$deffunc\n\
       \$classlevel\n\
       \#endif\n"
       (context [ ("typemacro"  , typemacrostr )
                , ("headers"    , headerStr    )
                , ("deffunc"    , deffunc      )
                , ("classlevel" , classlevel   )
                ])


-- |
buildFFIHsc :: ClassModule -> Module ()
buildFFIHsc m = mkModule (mname <.> "FFI") [lang ["ForeignFunctionInterface"]] ffiImports hscBody
  where mname = cmModule m
        headers = cmCIH m
        ffiImports = [ mkImport "Data.Word"
                     , mkImport "Data.Int"
                     , mkImport "Foreign.C"
                     , mkImport "Foreign.Ptr"
                     , mkImport (mname <.> "RawType") ]
                     <> genImportInFFI m
                     <> genExtraImport m
        hscBody = concatMap genHsFFI headers


-- |
buildRawTypeHs :: ClassModule -> Module ()
buildRawTypeHs m =
    mkModule
      (cmModule m <.> "RawType")
      [ lang
          [ "ForeignFunctionInterface"
          , "TypeFamilies"
          , "MultiParamTypeClasses"
          , "FlexibleInstances"
          , "TypeSynonymInstances"
          , "EmptyDataDecls"
          , "ExistentialQuantification"
          , "ScopedTypeVariables"
          ]
      ]
      rawtypeImports
      rawtypeBody
  where
    rawtypeImports = [ mkImport "Foreign.Ptr"
                     , mkImport "FFICXX.Runtime.Cast"
                     ]
    rawtypeBody = concatMap hsClassRawType . filter (not.isAbstractClass) . cmClass $ m

-- |
buildInterfaceHs :: AnnotateMap -> ClassModule -> Module ()
buildInterfaceHs amap m =
  mkModule
    (cmModule m <.> "Interface")
    [ lang
      [ "EmptyDataDecls"
      , "ExistentialQuantification"
      , "FlexibleContexts"
      , "FlexibleInstances"
      , "ForeignFunctionInterface"
      , "MultiParamTypeClasses"
      , "ScopedTypeVariables"
      , "TypeFamilies"
      , "TypeSynonymInstances"
      ]
    ]
    ifaceImports
    ifaceBody
  where
    classes = cmClass m
    ifaceImports = [ mkImport "Data.Word"
                   , mkImport "Data.Int"
                   , mkImport "Foreign.C"
                   , mkImport "Foreign.Ptr"
                   , mkImport "FFICXX.Runtime.Cast"
                   ]
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
                                        , "FlexibleContexts"
                                        , "FlexibleInstances"
                                        , "ForeignFunctionInterface"
                                        , "IncoherentInstances"
                                        , "MultiParamTypeClasses"
                                        , "OverlappingInstances"
                                        , "TemplateHaskell"
                                        , "TypeFamilies"
                                        , "TypeSynonymInstances"
                                        ] ]
                                 implImports implBody
  where classes = cmClass m
        implImports = [ mkImport "Data.Monoid"                -- for template member
                      , mkImport "Data.Word"
                      , mkImport "Data.Int"
                      , mkImport "Foreign.C"
                      , mkImport "Foreign.Ptr"
                      , mkImport "Language.Haskell.TH"        -- for template member
                      , mkImport "Language.Haskell.TH.Syntax" -- for template member
                      , mkImport "System.IO.Unsafe"
                      , mkImport "FFICXX.Runtime.Cast"
                      , mkImport "FFICXX.Runtime.TH"          -- for template member
                      ]
                      <> genImportInImplementation m
                      <> genExtraImport m
        f :: Class -> [Decl ()]
        f y = concatMap (flip genHsFrontInst y) (y:class_allparents y)

        implBody =    concatMap f classes
                   <> runReader (concat <$> mapM genHsFrontInstNew classes) amap
                   <> concatMap genHsFrontInstNonVirtual classes
                   <> concatMap genHsFrontInstStatic classes
                   <> concatMap genHsFrontInstVariables classes
                   <> concatMap genTemplateMemberFunctions classes

buildTemplateHs :: TemplateClassModule -> Module ()
buildTemplateHs m =
    mkModule (tcmModule m <.> "Template")
      [ lang
          [ "EmptyDataDecls"
          , "FlexibleInstances"
          , "MultiParamTypeClasses"
          , "TypeFamilies"
          ]
      ]
      [ mkImport "Foreign.C.Types"
      , mkImport "Foreign.Ptr"
      , mkImport "FFICXX.Runtime.Cast"
      ]
      body
  where
    ts = tcmTemplateClasses m
    body = concatMap genTmplInterface ts

buildTHHs :: TemplateClassModule -> Module ()
buildTHHs m =
  mkModule (tcmModule m <.> "TH")
    [ lang  ["TemplateHaskell"] ]
    (   [ mkImport "Data.Char"
        , mkImport "Data.Monoid"
        , mkImport "Foreign.C.Types"
        , mkImport "Foreign.Ptr"
        , mkImport "Language.Haskell.TH"
        , mkImport "Language.Haskell.TH.Syntax"
        , mkImport "FFICXX.Runtime.CodeGen.C"
        , mkImport "FFICXX.Runtime.TH"
        ]
     <> imports
    )
    body
  where
    ts = tcmTemplateClasses m
    imports = [ mkImport (tcmModule m <.> "Template") ]
    body = tmplImpls <> tmplInsts
    tmplImpls = concatMap genTmplImplementation ts
    tmplInsts = concatMap gen ts
      where
        gen t = do
          let tcihs = tcmTCIH m
          tcih <-
            maybeToList $
              find (\tcih -> tclass_name (tcihTClass tcih) == tclass_name t) tcihs
          genTmplInstance t tcih (tclass_funcs t)

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
