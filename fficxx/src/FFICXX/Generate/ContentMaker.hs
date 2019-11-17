{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.ContentMaker where

import Control.Lens                           ( (&), (.~), at )
import Control.Monad.Trans.Reader
import Data.Either                            ( rights )
import qualified Data.Map as M
import Data.Maybe                             ( mapMaybe, maybeToList )
import Data.Monoid                            ( (<>) )
import Data.List                              ( find, intercalate, nub )
import Data.List.Split                        ( splitOn )
import Language.Haskell.Exts.Syntax           ( Module(..)
                                              , Decl(..)
                                              )
import System.FilePath
--
{- import FFICXX.Runtime.CodeGen.C               ( CBlock(..)
                                              , CStatement(..)
                                              , CMacro
                                              , HeaderName(..)
                                              , PragmaParam(..)
                                              , renderCMacro
                                              , renderBlock
                                              ) -}
import FFICXX.Runtime.CodeGen.C               ( HeaderName(..) )
import qualified FFICXX.Runtime.CodeGen.C as R
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
                                              )
import FFICXX.Generate.Util                   ( connRet, connRet2, intercalateWith )
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
buildTypeDeclHeader ::
     [Class]
  -> String
buildTypeDeclHeader classes =
  let typeDeclBodyStmts =
        intercalate [R.EmptyLine] $
          map (map R.CRegular . genCppHeaderMacroType) classes
  in R.renderBlock $
       R.ExternC $
         [ R.Pragma R.Once, R.EmptyLine ] <> typeDeclBodyStmts


-- |
buildDeclHeader ::
     String     -- ^ C prefix
  -> ClassImportHeader
  -> String
buildDeclHeader cprefix header =
  let classes = [cihClass header]
      aclass = cihClass header
      declHeaderStmts =
           [ R.Include (HdrName (cprefix ++ "Type.h")) ]
        <> map R.Include (cihIncludedHPkgHeadersInH header)
      vdef  = intercalate "\n\n" $
                map (concat . map R.renderCMacro . genCppHeaderMacroVirtual) classes
      nvdef = intercalate "\n\n" $
                map (concat . map R.renderCMacro . genCppHeaderMacroNonVirtual) classes
      acdef = intercalate "\n\n" $
                map (concat . map R.renderCMacro . genCppHeaderMacroAccessor) classes

      declDefStr = vdef
                   `connRet2`
                   nvdef
                   `connRet2`
                   acdef
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
  in R.renderBlock $
       R.ExternC $
            [ R.Pragma R.Once, R.EmptyLine ]
         <> declHeaderStmts
         <> [ R.EmptyLine
            , R.Verbatim declBodyStr
            ]

-- |
buildDefMain :: ClassImportHeader
          -> String
buildDefMain cih =
  let classes = [cihClass cih]
      headerStmts =
           [ R.Include "MacroPatternMatch.h" ]
        <> genAllCppHeaderInclude cih
        <> [ R.Include (cihSelfHeader cih) ]
      namespaceStmts =
        (map R.UsingNamespace . cihNamespace) cih
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
  in concatMap R.renderCMacro
       (   headerStmts
        <> [ R.EmptyLine ]
        <> map R.CRegular namespaceStmts
        <> [ R.EmptyLine
           , R.Verbatim aliasStr
           , R.EmptyLine
           , R.Verbatim "#define CHECKPROTECT(x,y) IS_PAREN(IS_ ## x ## _ ## y ## _PROTECTED)\n"
           , R.EmptyLine
           , R.Verbatim "#define TYPECASTMETHOD(cname,mname,oname) \\\n\
                        \  IIF( CHECKPROTECT(cname,mname) ) ( \\\n\
                        \  (to_nonconst<oname,cname ## _t>), \\\n\
                        \  (to_nonconst<cname,cname ## _t>) )\n"
           , R.EmptyLine
           , R.Verbatim cppBody
           ]
       )

-- |
buildTopLevelHeader ::
     String     -- ^ C prefix
  -> TopLevelImportHeader
  -> String
buildTopLevelHeader cprefix tih =
  let declHeaderStmts =
           [ R.Include (HdrName (cprefix ++ "Type.h")) ]
        <> map R.Include (map cihSelfHeader (tihClassDep tih) ++ tihExtraHeadersInH tih)
      declBodyStmts = map genTopLevelFuncCppHeader (tihFuncs tih)
  in R.renderBlock $
       R.ExternC $
            [ R.Pragma R.Once, R.EmptyLine ]
         <> declHeaderStmts
         <> [ R.EmptyLine ]
         <> map R.CRegular declBodyStmts

-- |
buildTopLevelCppDef :: TopLevelImportHeader -> String
buildTopLevelCppDef tih =
  let cihs = tihClassDep tih
      extclasses = tihExtraClassDep tih
      declHeaderStmts =
           [ R.Include "MacroPatternMatch.h"
           , R.Include (HdrName (tihHeaderFileName tih <.> "h"))
           ]
        <> concatMap genAllCppHeaderInclude cihs
        <> otherHeaderStmts
      otherHeaderStmts =
        map R.Include (map cihSelfHeader cihs ++ tihExtraHeadersInCPP tih)

      allns = nub ((tihClassDep tih >>= cihNamespace) ++ tihNamespaces tih)

      namespaceStmts = map R.UsingNamespace allns
      aliasStr = intercalate "\n" $
                   mapMaybe typedefstmt $
                     rights (concatMap cihImportedClasses cihs ++ extclasses)
        where typedefstmt c = let n1 = class_name c
                                  n2 = ffiClassName c
                              in if n1 == n2
                                 then Nothing
                                 else Just ("typedef " <> n1 <> " " <> n2 <> ";")
      declBodyStr    = intercalateWith connRet genTopLevelFuncCppDefinition (tihFuncs tih)

  in concatMap R.renderCMacro
       (   declHeaderStmts
        <> [R.EmptyLine]
        <> map R.CRegular namespaceStmts
        <> [ R.EmptyLine
           , R.Verbatim aliasStr
           , R.EmptyLine
           , R.Verbatim "#define CHECKPROTECT(x,y) IS_PAREN(IS_ ## x ## _ ## y ## _PROTECTED)\n"
           , R.EmptyLine
           , R.Verbatim "#define TYPECASTMETHOD(cname,mname,oname) \\\n\
                        \  IIF( CHECKPROTECT(cname,mname) ) ( \\\n\
                        \  (to_nonconst<oname,cname ## _t>), \\\n\
                        \  (to_nonconst<cname,cname ## _t>) )\n"
           , R.EmptyLine
           , R.Verbatim declBodyStr
           ]
       )

-- |
buildTemplateHeader ::
     TemplateClassImportHeader
  -> String
buildTemplateHeader tcih =
  let t = tcihTClass tcih
      fs = tclass_funcs t
      headerStmts = map R.Include (tcihCxxHeaders tcih)
      deffunc = intercalateWith connRet (genTmplFunCpp False t) fs
                ++ "\n\n"
                ++ intercalateWith connRet (genTmplFunCpp True t) fs
      classlevel = genTmplClassCpp False t fs ++ "\n\n" ++ genTmplClassCpp True t fs
  in concatMap R.renderCMacro $
          [ R.Pragma R.Once
          , R.EmptyLine
          ]
       <> headerStmts
       <> [ R.EmptyLine
          , R.Verbatim deffunc
          , R.EmptyLine
          , R.Verbatim classlevel
          ]

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
