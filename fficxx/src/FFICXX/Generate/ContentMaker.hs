{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.ContentMaker where

import Control.Lens (at, (&), (.~))
import Control.Monad.Trans.Reader (runReader)
import Data.Either (rights)
import Data.Functor.Identity (Identity)
import Data.List (intercalate, nub, singleton)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import FFICXX.Generate.Code.Cpp
  ( genAllCppHeaderInclude,
    genCppDefInstAccessor,
    genCppDefInstNonVirtual,
    genCppDefInstVirtual,
    genCppDefMacroAccessor,
    genCppDefMacroNonVirtual,
    genCppDefMacroTemplateMemberFunction,
    genCppDefMacroVirtual,
    genCppHeaderInstAccessor,
    genCppHeaderInstNonVirtual,
    genCppHeaderInstVirtual,
    genCppHeaderMacroAccessor,
    genCppHeaderMacroNonVirtual,
    genCppHeaderMacroType,
    genCppHeaderMacroVirtual,
    genTopLevelCppDefinition,
    topLevelDecl,
  )
import FFICXX.Generate.Code.HsCast
  ( genHsFrontInstCastable,
    genHsFrontInstCastableSelf,
    genImportInCast,
  )
import FFICXX.Generate.Code.HsCommon
  ( genExtraImport,
  )
import FFICXX.Generate.Code.HsEnum
  ( genHsEnumDecl,
    -- genHsEnumFFI,
    genHsEnumInclude,
  )
import FFICXX.Generate.Code.HsFFI
  ( genHsFFI,
    genImportInFFI,
    genTopLevelFFI,
  )
import FFICXX.Generate.Code.HsImplementation
  ( genHsFrontInst,
    genHsFrontInstNew,
    genHsFrontInstNonVirtual,
    genHsFrontInstStatic,
    genHsFrontInstVariables,
    genImportInImplementation,
    genTemplateMemberFunctions,
  )
import FFICXX.Generate.Code.HsInterface
  ( genHsFrontDecl,
    genHsFrontDowncastClass,
    genHsFrontUpcastClass,
    genImportInInterface,
  )
import FFICXX.Generate.Code.HsProxy (genProxyInstance)
import FFICXX.Generate.Code.HsRawType (hsClassRawType)
import FFICXX.Generate.Code.HsTH
  ( genImportInTH,
    genTmplImplementation,
    genTmplInstance,
  )
import FFICXX.Generate.Code.HsTemplate
  ( genImportInTemplate,
    genTmplInterface,
  )
import FFICXX.Generate.Code.HsTopLevel
  ( genExport,
    genImportForTLOrdinary,
    genImportForTLTemplate,
    genImportInModule,
    genImportInTopLevel,
    genTLTemplateImplementation,
    genTLTemplateInstance,
    genTLTemplateInterface,
    genTopLevelDef,
  )
import FFICXX.Generate.Dependency
  ( class_allparents,
    mkDaughterMap,
    mkDaughterSelfMap,
  )
import FFICXX.Generate.Name
  ( ffiClassName,
    hsClassName,
    hsFrontNameForTopLevel,
  )
import FFICXX.Generate.Type.Annotate (AnnotateMap)
import FFICXX.Generate.Type.Class
  ( Class (..),
    ClassGlobal (..),
    DaughterMap,
    EnumType (..),
    ProtectedMethod (..),
    TopLevel (TLOrdinary, TLTemplate),
    filterTLOrdinary,
    filterTLTemplate,
    isAbstractClass,
  )
import FFICXX.Generate.Type.Module
  ( ClassImportHeader (..),
    ClassModule (..),
    DepCycles,
    TemplateClassImportHeader (..),
    TemplateClassModule (..),
    TopLevelImportHeader (..),
  )
import FFICXX.Generate.Type.PackageInterface
  ( ClassName (..),
    PackageInterface,
    PackageName (..),
  )
import FFICXX.Generate.Util (firstUpper)
import qualified FFICXX.Generate.Util.GHCExactPrint as Ex
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import GHC.Hs.Extension (GhcPs)
import Language.Haskell.Syntax
  ( HsDecl,
    HsModule,
  )
import System.FilePath ((<.>), (</>))

srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src"

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc"

---- common function for daughter

mkGlobal :: [Class] -> ClassGlobal
mkGlobal = ClassGlobal <$> mkDaughterSelfMap <*> mkDaughterMap

buildDaughterDef ::
  ((String, [Class]) -> String) ->
  DaughterMap ->
  String
buildDaughterDef f m =
  let lst = M.toList m
      f' (x, xs) = f (x, filter (not . isAbstractClass) xs)
   in (concatMap f' lst)

buildParentDef :: ((Class, Class) -> [R.CStatement Identity]) -> Class -> [R.CStatement Identity]
buildParentDef f cls = concatMap (\p -> f (p, cls)) . class_allparents $ cls

mkProtectedFunctionList :: Class -> [R.CMacro Identity]
mkProtectedFunctionList c =
  map (\x -> R.Define (R.sname ("IS_" <> class_name c <> "_" <> x <> "_PROTECTED")) [] [R.CVerbatim "()"])
    . unProtected
    . class_protected
    $ c

buildTypeDeclHeader ::
  [Class] ->
  String
buildTypeDeclHeader classes =
  let typeDeclBodyStmts =
        intercalate [R.EmptyLine] $
          map (map R.CRegular . genCppHeaderMacroType) classes
   in R.renderBlock $
        R.ExternC $
          [R.Pragma R.Once, R.EmptyLine] <> typeDeclBodyStmts

buildDeclHeader ::
  -- | C prefix
  String ->
  ClassImportHeader ->
  String
buildDeclHeader cprefix header =
  let classes = [cihClass header]
      aclass = cihClass header
      declHeaderStmts =
        [R.Include (HdrName (cprefix ++ "Type.h"))]
          <> map R.Include (cihIncludedHPkgHeadersInH header)
      vdecl = map genCppHeaderMacroVirtual classes
      nvdecl = map genCppHeaderMacroNonVirtual classes
      acdecl = map genCppHeaderMacroAccessor classes
      vdef = map genCppDefMacroVirtual classes
      nvdef = map genCppDefMacroNonVirtual classes
      acdef = map genCppDefMacroAccessor classes
      tmpldef = map (\c -> map (genCppDefMacroTemplateMemberFunction c) (class_tmpl_funcs c)) classes
      declDefStmts =
        intercalate [R.EmptyLine] $ [vdecl, nvdecl, acdecl, vdef, nvdef, acdef] ++ tmpldef
      classDeclStmts =
        -- NOTE: Deletable is treated specially.
        -- TODO: We had better make it as a separate constructor in Class.
        if (fst . hsClassName) aclass /= "Deletable"
          then
            buildParentDef (\(p, c) -> [genCppHeaderInstVirtual (p, c), R.CEmptyLine]) aclass
              <> [genCppHeaderInstVirtual (aclass, aclass), R.CEmptyLine]
              <> concatMap (\c -> [genCppHeaderInstNonVirtual c, R.CEmptyLine]) classes
              <> concatMap (\c -> [genCppHeaderInstAccessor c, R.CEmptyLine]) classes
          else []
   in R.renderBlock $
        R.ExternC $
          [R.Pragma R.Once, R.EmptyLine]
            <> declHeaderStmts
            <> [R.EmptyLine]
            <> declDefStmts
            <> [R.EmptyLine]
            <> map R.CRegular classDeclStmts

buildDefMain ::
  ClassImportHeader ->
  String
buildDefMain cih =
  let classes = [cihClass cih]
      headerStmts =
        [R.Include "MacroPatternMatch.h"]
          <> genAllCppHeaderInclude cih
          <> [R.Include (cihSelfHeader cih)]
      namespaceStmts =
        (map R.UsingNamespace . cihNamespace) cih
      aclass = cihClass cih
      aliasStr =
        intercalate "\n" $
          mapMaybe typedefstmt $
            aclass : rights (cihImportedClasses cih)
        where
          typedefstmt c =
            let n1 = class_name c
                n2 = ffiClassName c
             in if n1 == n2
                  then Nothing
                  else Just ("typedef " <> n1 <> " " <> n2 <> ";")
      cppBodyStmts =
        mkProtectedFunctionList (cihClass cih)
          <> map
            R.CRegular
            ( buildParentDef (\(p, c) -> [genCppDefInstVirtual (p, c), R.CEmptyLine]) (cihClass cih)
                <> ( if isAbstractClass aclass
                       then []
                       else [genCppDefInstVirtual (aclass, aclass), R.CEmptyLine]
                   )
                <> concatMap (\c -> [genCppDefInstNonVirtual c, R.CEmptyLine]) classes
                <> concatMap (\c -> [genCppDefInstAccessor c, R.CEmptyLine]) classes
            )
   in concatMap
        R.renderCMacro
        ( headerStmts
            <> [R.EmptyLine]
            <> map R.CRegular namespaceStmts
            <> [ R.EmptyLine,
                 R.Verbatim aliasStr,
                 R.EmptyLine,
                 R.Verbatim "#define CHECKPROTECT(x,y) FXIS_PAREN(IS_ ## x ## _ ## y ## _PROTECTED)\n",
                 R.EmptyLine,
                 R.Verbatim
                   "#define TYPECASTMETHOD(cname,mname,oname) \\\n\
                   \  FXIIF( CHECKPROTECT(cname,mname) ) ( \\\n\
                   \  (from_nonconst_to_nonconst<oname,cname ## _t>), \\\n\
                   \  (from_nonconst_to_nonconst<cname,cname ## _t>) )\n",
                 R.EmptyLine
               ]
            <> cppBodyStmts
        )

buildTopLevelHeader ::
  -- | C prefix
  String ->
  TopLevelImportHeader ->
  String
buildTopLevelHeader cprefix tih =
  let declHeaderStmts =
        [R.Include (HdrName (cprefix ++ "Type.h"))]
          <> map R.Include (map cihSelfHeader (tihClassDep tih) ++ tihExtraHeadersInH tih)
      declBodyStmts = map (R.CDeclaration . topLevelDecl) $ filterTLOrdinary (tihFuncs tih)
   in R.renderBlock $
        R.ExternC $
          [R.Pragma R.Once, R.EmptyLine]
            <> declHeaderStmts
            <> [R.EmptyLine]
            <> map R.CRegular declBodyStmts

buildTopLevelCppDef :: TopLevelImportHeader -> String
buildTopLevelCppDef tih =
  let cihs = tihClassDep tih
      extclasses = tihExtraClassDep tih
      declHeaderStmts =
        [ R.Include "MacroPatternMatch.h",
          R.Include (HdrName (tihHeaderFileName tih <.> "h"))
        ]
          <> concatMap genAllCppHeaderInclude cihs
          <> otherHeaderStmts
      otherHeaderStmts =
        map R.Include (map cihSelfHeader cihs ++ tihExtraHeadersInCPP tih)
      allns = nub ((tihClassDep tih >>= cihNamespace) ++ tihNamespaces tih)
      namespaceStmts = map R.UsingNamespace allns
      aliasStr =
        intercalate "\n" $
          mapMaybe typedefstmt $
            rights (concatMap cihImportedClasses cihs ++ extclasses)
        where
          typedefstmt c =
            let n1 = class_name c
                n2 = ffiClassName c
             in if n1 == n2
                  then Nothing
                  else Just ("typedef " <> n1 <> " " <> n2 <> ";")
      declBodyStr =
        intercalate "\n" $
          map (R.renderCStmt . genTopLevelCppDefinition) $
            filterTLOrdinary (tihFuncs tih)
   in concatMap
        R.renderCMacro
        ( declHeaderStmts
            <> [R.EmptyLine]
            <> map R.CRegular namespaceStmts
            <> [ R.EmptyLine,
                 R.Verbatim aliasStr,
                 R.EmptyLine,
                 R.Verbatim "#define CHECKPROTECT(x,y) FXIS_PAREN(IS_ ## x ## _ ## y ## _PROTECTED)\n",
                 R.EmptyLine,
                 R.Verbatim
                   "#define TYPECASTMETHOD(cname,mname,oname) \\\n\
                   \  FXIIF( CHECKPROTECT(cname,mname) ) ( \\\n\
                   \  (to_nonconst<oname,cname ## _t>), \\\n\
                   \  (to_nonconst<cname,cname ## _t>) )\n",
                 R.EmptyLine,
                 R.Verbatim declBodyStr
               ]
        )

buildFFIHsc :: ClassModule -> HsModule GhcPs
buildFFIHsc m =
  Ex.mkModule
    (mname <.> "FFI")
    ["ForeignFunctionInterface", "InterruptibleFFI"]
    ffiImports
    hscBody
  where
    mname = cmModule m
    ffiImports =
      [ Ex.mkImport "Data.Word",
        Ex.mkImport "Data.Int",
        Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport (mname <.> "RawType")
      ]
        <> genImportInFFI m
        <> genExtraImport m
    hscBody = singleton $ Ex.DeclGroup $ fmap Ex.forD (genHsFFI (cmCIH m))

buildRawTypeHs :: ClassModule -> HsModule GhcPs
buildRawTypeHs m =
  Ex.mkModule
    (cmModule m <.> "RawType")
    [ "ForeignFunctionInterface",
      "TypeFamilies",
      "MultiParamTypeClasses",
      "FlexibleInstances",
      "TypeSynonymInstances",
      "EmptyDataDecls",
      "ExistentialQuantification",
      "ScopedTypeVariables"
    ]
    rawtypeImports
    rawtypeBody
  where
    rawtypeImports =
      [ Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "FFICXX.Runtime.Cast"
      ]
    rawtypeBody =
      singleton . Ex.DeclGroup $
        let c = cihClass (cmCIH m)
         in if isAbstractClass c then [] else hsClassRawType c

buildInterfaceHs ::
  AnnotateMap ->
  DepCycles ->
  ClassModule ->
  HsModule GhcPs
buildInterfaceHs amap depCycles m =
  Ex.mkModule
    (cmModule m <.> "Interface")
    [ "EmptyDataDecls",
      "ExistentialQuantification",
      "FlexibleContexts",
      "FlexibleInstances",
      "ForeignFunctionInterface",
      "MultiParamTypeClasses",
      "ScopedTypeVariables",
      "TypeFamilies",
      "TypeSynonymInstances"
    ]
    ifaceImports
    ifaceBody
  where
    classes = [cihClass (cmCIH m)]
    ifaceImports =
      [ Ex.mkImport "Data.Word",
        Ex.mkImport "Data.Int",
        Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "FFICXX.Runtime.Cast"
      ]
        <> genImportInInterface False depCycles m
        <> genExtraImport m
    ifaceBody =
      singleton . Ex.DeclGroup $
        runReader (traverse (genHsFrontDecl False) classes) amap
          <> (concatMap genHsFrontUpcastClass . filter (not . isAbstractClass)) classes
          <> (concatMap genHsFrontDowncastClass . filter (not . isAbstractClass)) classes

buildInterfaceHsBoot :: DepCycles -> ClassModule -> HsModule GhcPs
buildInterfaceHsBoot depCycles m =
  Ex.mkModule
    (cmModule m <.> "Interface")
    [ "EmptyDataDecls",
      "ExistentialQuantification",
      "FlexibleContexts",
      "FlexibleInstances",
      "ForeignFunctionInterface",
      "MultiParamTypeClasses",
      "ScopedTypeVariables",
      "TypeFamilies",
      "TypeSynonymInstances"
    ]
    hsbootImports
    hsbootBody
  where
    c = cihClass (cmCIH m)
    hsbootImports =
      [ Ex.mkImport "Data.Word",
        Ex.mkImport "Data.Int",
        Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "FFICXX.Runtime.Cast"
      ]
        <> genImportInInterface True depCycles m
        <> genExtraImport m
    hsbootBody =
      singleton . Ex.DeclGroup $ runReader (mapM (genHsFrontDecl True) [c]) M.empty

buildCastHs :: ClassModule -> HsModule GhcPs
buildCastHs m =
  Ex.mkModule
    (cmModule m <.> "Cast")
    [ "FlexibleInstances",
      "FlexibleContexts",
      "TypeFamilies",
      "MultiParamTypeClasses",
      "OverlappingInstances",
      "IncoherentInstances"
    ]
    castImports
    body
  where
    classes = [cihClass (cmCIH m)]
    castImports =
      [ Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "FFICXX.Runtime.Cast",
        Ex.mkImport "System.IO.Unsafe"
      ]
        <> genImportInCast m
    body =
      singleton . Ex.DeclGroup $
        mapMaybe genHsFrontInstCastable classes
          <> mapMaybe genHsFrontInstCastableSelf classes

buildImplementationHs :: AnnotateMap -> ClassModule -> HsModule GhcPs
buildImplementationHs amap m =
  Ex.mkModule
    (cmModule m <.> "Implementation")
    [ "EmptyDataDecls",
      "FlexibleContexts",
      "FlexibleInstances",
      "ForeignFunctionInterface",
      "IncoherentInstances",
      "MultiParamTypeClasses",
      "OverlappingInstances",
      "TemplateHaskell",
      "TypeFamilies",
      "TypeSynonymInstances"
    ]
    implImports
    implBody
  where
    classes = [cihClass (cmCIH m)]
    implImports =
      [ Ex.mkImport "Data.Monoid", -- for template member
        Ex.mkImport "Data.Word",
        Ex.mkImport "Data.Int",
        Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "Language.Haskell.TH", -- for template member
        Ex.mkImport "Language.Haskell.TH.Syntax", -- for template member
        Ex.mkImport "System.IO.Unsafe",
        Ex.mkImport "FFICXX.Runtime.Cast",
        Ex.mkImport "FFICXX.Runtime.CodeGen.Cxx", -- for template member
        Ex.mkImport "FFICXX.Runtime.TH" -- for template member
      ]
        <> genImportInImplementation m
        <> genExtraImport m

    f :: Class -> [HsDecl GhcPs]
    f y = concatMap (flip genHsFrontInst y) (y : class_allparents y)
    implBody =
      singleton . Ex.DeclGroup $
        concatMap f classes
          <> runReader (concat <$> mapM genHsFrontInstNew classes) amap
          <> concatMap genHsFrontInstNonVirtual classes
          <> concatMap genHsFrontInstStatic classes
          <> concatMap genHsFrontInstVariables classes
          <> genTemplateMemberFunctions (cmCIH m)

buildProxyHs :: ClassModule -> HsModule GhcPs
buildProxyHs m =
  Ex.mkModule
    (cmModule m <.> "Proxy")
    [ "FlexibleInstances",
      "OverloadedStrings",
      "TemplateHaskell"
    ]
    [ Ex.mkImport "Foreign.Ptr",
      Ex.mkImport "FFICXX.Runtime.Cast",
      Ex.mkImport "Language.Haskell.TH",
      Ex.mkImport "Language.Haskell.TH.Syntax",
      Ex.mkImport "FFICXX.Runtime.CodeGen.Cxx"
    ]
    body
  where
    body = singleton . Ex.DeclGroup $ genProxyInstance

buildTemplateHs :: TemplateClassModule -> HsModule GhcPs
buildTemplateHs m =
  Ex.mkModule
    (tcmModule m <.> "Template")
    [ "EmptyDataDecls",
      "FlexibleInstances",
      "MultiParamTypeClasses",
      "TypeFamilies"
    ]
    imports
    body
  where
    t = tcihTClass $ tcmTCIH m
    imports =
      [ Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "FFICXX.Runtime.Cast"
      ]
        <> genImportInTemplate t
    body = singleton . Ex.DeclGroup $ genTmplInterface t

buildTHHs :: TemplateClassModule -> HsModule GhcPs
buildTHHs m =
  Ex.mkModule
    (tcmModule m <.> "TH")
    ["TemplateHaskell"]
    ( [ Ex.mkImport "Data.Char",
        Ex.mkImport "Data.List",
        Ex.mkImport "Data.Monoid",
        Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "Language.Haskell.TH",
        Ex.mkImport "Language.Haskell.TH.Syntax",
        Ex.mkImport "FFICXX.Runtime.CodeGen.Cxx",
        Ex.mkImport "FFICXX.Runtime.TH"
      ]
        <> imports
    )
    body
  where
    t = tcihTClass $ tcmTCIH m
    imports =
      [Ex.mkImport (tcmModule m <.> "Template")]
        <> genImportInTH t
    tmplImpls = genTmplImplementation t
    tmplInsts = genTmplInstance (tcmTCIH m)
    body =
      singleton . Ex.DeclGroup $
        tmplImpls <> tmplInsts

buildModuleHs :: ClassModule -> HsModule GhcPs
buildModuleHs m =
  Ex.mkModuleE
    (cmModule m)
    []
    (Just (genExport c))
    (genImportInModule c)
    (singleton . Ex.DeclGroup $ [])
  where
    c = cihClass (cmCIH m)

buildEnumHsc ::
  AnnotateMap ->
  String ->
  [EnumType] ->
  HsModule GhcPs
buildEnumHsc amap modname enums =
  Ex.mkModuleE modname [] Nothing [] body
  where
    body =
      concatMap
        ( \enum ->
            [ genHsEnumInclude enum,
              Ex.DeclGroup (genHsEnumDecl enum)
              -- , genHsEnumFFI enum])
            ]
        )
        enums

buildTopLevelHs ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  HsModule GhcPs
buildTopLevelHs modname (mods, tmods) =
  Ex.mkModuleE modname pkgExtensions (Just pkgExports) pkgImports pkgBody
  where
    pkgExtensions =
      [ "FlexibleContexts",
        "FlexibleInstances",
        "ForeignFunctionInterface",
        "InterruptibleFFI"
      ]
    pkgExports =
      map (Ex.emodule . cmModule) mods
        ++ map Ex.emodule [modname <.> "Ordinary", modname <.> "Template", modname <.> "TH"]
    pkgImports = genImportInTopLevel modname (mods, tmods)
    pkgBody = singleton . Ex.DeclGroup $ []

buildTopLevelOrdinaryHs ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  TopLevelImportHeader ->
  HsModule GhcPs
buildTopLevelOrdinaryHs modname (_mods, tmods) tih =
  Ex.mkModuleE modname pkgExtensions (Just pkgExports) pkgImports pkgBody
  where
    tfns = tihFuncs tih
    pkgExtensions =
      [ "FlexibleContexts",
        "FlexibleInstances",
        "ForeignFunctionInterface",
        "InterruptibleFFI"
      ]
    pkgExports = fmap (Ex.evar . hsFrontNameForTopLevel . TLOrdinary) (filterTLOrdinary tfns)
    pkgImports =
      fmap Ex.mkImport ["Foreign.C", "Foreign.Ptr", "FFICXX.Runtime.Cast"]
        ++ fmap (\m -> Ex.mkImport (tcmModule m <.> "Template")) tmods
        ++ concatMap genImportForTLOrdinary (filterTLOrdinary tfns)
    pkgBody =
      singleton . Ex.DeclGroup $
        map (Ex.forD . genTopLevelFFI tih) (filterTLOrdinary tfns)
          ++ concatMap genTopLevelDef (filterTLOrdinary tfns)

buildTopLevelTemplateHs ::
  String ->
  TopLevelImportHeader ->
  HsModule GhcPs
buildTopLevelTemplateHs modname tih =
  Ex.mkModuleE modname pkgExtensions (Just pkgExports) pkgImports pkgBody
  where
    tfns = filterTLTemplate (tihFuncs tih)
    pkgExtensions =
      [ "EmptyDataDecls",
        "FlexibleInstances",
        "ForeignFunctionInterface",
        "InterruptibleFFI",
        "MultiParamTypeClasses",
        "TypeFamilies"
      ]
    pkgExports =
      map
        ( (\n -> Ex.ethingall n)
            . firstUpper
            . hsFrontNameForTopLevel
            . TLTemplate
        )
        tfns
    pkgImports =
      [ Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "FFICXX.Runtime.Cast"
      ]
        ++ concatMap genImportForTLTemplate tfns
    pkgBody = singleton . Ex.DeclGroup $ concatMap genTLTemplateInterface tfns

buildTopLevelTHHs ::
  String ->
  TopLevelImportHeader ->
  HsModule GhcPs
buildTopLevelTHHs modname tih =
  Ex.mkModuleE modname pkgExtensions (Just pkgExports) pkgImports pkgBody
  where
    tfns = filterTLTemplate (tihFuncs tih)
    pkgExtensions =
      [ "FlexibleContexts",
        "FlexibleInstances",
        "ForeignFunctionInterface",
        "InterruptibleFFI",
        "TemplateHaskell"
      ]
    pkgExports =
      map
        ( Ex.evar
            . (\x -> "gen" <> x <> "InstanceFor")
            . firstUpper
            . hsFrontNameForTopLevel
            . TLTemplate
        )
        tfns
    pkgImports =
      [ Ex.mkImport "Data.Char",
        Ex.mkImport "Data.List",
        Ex.mkImport "Data.Monoid",
        Ex.mkImport "Foreign.C",
        Ex.mkImport "Foreign.Ptr",
        Ex.mkImport "Language.Haskell.TH",
        Ex.mkImport "Language.Haskell.TH.Syntax",
        Ex.mkImport "FFICXX.Runtime.CodeGen.Cxx",
        Ex.mkImport "FFICXX.Runtime.TH"
      ]
        ++ concatMap genImportForTLTemplate tfns
    pkgBody =
      singleton . Ex.DeclGroup $
        concatMap genTLTemplateImplementation tfns
          <> concatMap (genTLTemplateInstance tih) tfns

buildPackageInterface ::
  PackageInterface ->
  PackageName ->
  [ClassImportHeader] ->
  PackageInterface
buildPackageInterface pinfc pkgname = foldr f pinfc
  where
    f cih repo =
      let name = (class_name . cihClass) cih
          header = cihSelfHeader cih
       in repo & at (pkgname, ClsName name) .~ (Just header)
