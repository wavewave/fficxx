{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module FFICXX.Generate.ContentMaker where

import Control.Lens (at, (&), (.~))
import Control.Monad.Trans.Reader (runReader)
import Data.Either (rights)
import Data.Functor.Identity (Identity)
import Data.List (intercalate, nub)
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
  )
import FFICXX.Generate.Code.HsFFI
  ( genHsFFI,
    genImportInFFI,
    genTopLevelFFI_,
  )
import FFICXX.Generate.Code.HsFrontEnd
  ( genExport,
    genExtraImport,
    genExtraImport_,
    genImportForTLOrdinary,
    genImportForTLTemplate,
    genImportInCast,
    genImportInModule,
    genImportInTopLevel,
    genTopLevelDef,
  )
import FFICXX.Generate.Code.HsImplementation
  ( genHsFrontInst,
    genHsFrontInstNew,
    genHsFrontInstNonVirtual,
    genHsFrontInstStatic,
    genHsFrontInstVariables,
    genImportInImplementation,
  )
import FFICXX.Generate.Code.HsInterface
  ( genHsFrontDecl,
    genHsFrontDowncastClass,
    genHsFrontUpcastClass,
    genImportInInterface,
  )
import FFICXX.Generate.Code.HsProxy (genProxyInstance)
import FFICXX.Generate.Code.HsRawType (hsClassRawType)
import FFICXX.Generate.Code.HsTemplate
  ( genImportInTH,
    genImportInTemplate,
    genTLTemplateImplementation,
    genTLTemplateInstance,
    genTLTemplateInterface,
    genTemplateMemberFunctions,
    genTmplImplementation,
    genTmplInstance,
    genTmplInterface,
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
import FFICXX.Generate.Util.HaskellSrcExts
  ( eWildCard,
    emodule,
    ethingwith,
    evar,
    lang,
    mkImport,
    mkModule,
    mkModuleE,
    unqual,
  )
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import GHC.Hs.Extension (GhcPs)
import Language.Haskell.Exts.Syntax
  ( Decl,
    Module,
  )
import Language.Haskell.Syntax
  ( HsDecl (ForD),
    HsModule,
    noExtField,
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
    hscBody = fmap (ForD noExtField) (genHsFFI (cmCIH m))

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
      let c = cihClass (cmCIH m)
       in if isAbstractClass c then [] else hsClassRawType c

buildInterfaceHs ::
  AnnotateMap ->
  DepCycles ->
  ClassModule ->
  Module ()
buildInterfaceHs amap depCycles m =
  mkModule
    (cmModule m <.> "Interface")
    [ lang
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
    ]
    ifaceImports
    ifaceBody
  where
    classes = [cihClass (cmCIH m)]
    ifaceImports =
      [ mkImport "Data.Word",
        mkImport "Data.Int",
        mkImport "Foreign.C",
        mkImport "Foreign.Ptr",
        mkImport "FFICXX.Runtime.Cast"
      ]
        <> genImportInInterface False depCycles m
        <> genExtraImport_ m
    ifaceBody =
      runReader (mapM (genHsFrontDecl False) classes) amap
        <> (concatMap genHsFrontUpcastClass . filter (not . isAbstractClass)) classes
        <> (concatMap genHsFrontDowncastClass . filter (not . isAbstractClass)) classes

buildInterfaceHsBoot :: DepCycles -> ClassModule -> Module ()
buildInterfaceHsBoot depCycles m =
  mkModule
    (cmModule m <.> "Interface")
    [ lang
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
    ]
    hsbootImports
    hsbootBody
  where
    c = cihClass (cmCIH m)
    hsbootImports =
      [ mkImport "Data.Word",
        mkImport "Data.Int",
        mkImport "Foreign.C",
        mkImport "Foreign.Ptr",
        mkImport "FFICXX.Runtime.Cast"
      ]
        <> genImportInInterface True depCycles m
        <> genExtraImport_ m
    hsbootBody =
      runReader (mapM (genHsFrontDecl True) [c]) M.empty

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
      mapMaybe genHsFrontInstCastable classes
        <> mapMaybe genHsFrontInstCastableSelf classes

buildImplementationHs :: AnnotateMap -> ClassModule -> Module ()
buildImplementationHs amap m =
  mkModule
    (cmModule m <.> "Implementation")
    [ lang
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
    ]
    implImports
    implBody
  where
    classes = [cihClass (cmCIH m)]
    implImports =
      [ mkImport "Data.Monoid", -- for template member
        mkImport "Data.Word",
        mkImport "Data.Int",
        mkImport "Foreign.C",
        mkImport "Foreign.Ptr",
        mkImport "Language.Haskell.TH", -- for template member
        mkImport "Language.Haskell.TH.Syntax", -- for template member
        mkImport "System.IO.Unsafe",
        mkImport "FFICXX.Runtime.Cast",
        mkImport "FFICXX.Runtime.CodeGen.Cxx", -- for template member
        mkImport "FFICXX.Runtime.TH" -- for template member
      ]
        <> genImportInImplementation m
        <> genExtraImport_ m
    f :: Class -> [Decl ()]
    f y = concatMap (flip genHsFrontInst y) (y : class_allparents y)
    implBody =
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
    body = genProxyInstance

buildTemplateHs :: TemplateClassModule -> Module ()
buildTemplateHs m =
  mkModule
    (tcmModule m <.> "Template")
    [ lang
        [ "EmptyDataDecls",
          "FlexibleInstances",
          "MultiParamTypeClasses",
          "TypeFamilies"
        ]
    ]
    imports
    body
  where
    t = tcihTClass $ tcmTCIH m
    imports =
      [ mkImport "Foreign.C.Types",
        mkImport "Foreign.Ptr",
        mkImport "FFICXX.Runtime.Cast"
      ]
        <> genImportInTemplate t
    body = genTmplInterface t

buildTHHs :: TemplateClassModule -> Module ()
buildTHHs m =
  mkModule
    (tcmModule m <.> "TH")
    [lang ["TemplateHaskell"]]
    ( [ mkImport "Data.Char",
        mkImport "Data.List",
        mkImport "Data.Monoid",
        mkImport "Foreign.C.Types",
        mkImport "Foreign.Ptr",
        mkImport "Language.Haskell.TH",
        mkImport "Language.Haskell.TH.Syntax",
        mkImport "FFICXX.Runtime.CodeGen.Cxx",
        mkImport "FFICXX.Runtime.TH"
      ]
        <> imports
    )
    body
  where
    t = tcihTClass $ tcmTCIH m
    imports =
      [mkImport (tcmModule m <.> "Template")]
        <> genImportInTH t
    body = tmplImpls <> tmplInsts
    tmplImpls = genTmplImplementation t
    tmplInsts = genTmplInstance (tcmTCIH m)

buildModuleHs :: ClassModule -> Module ()
buildModuleHs m = mkModuleE (cmModule m) [] (genExport c) (genImportInModule c) []
  where
    c = cihClass (cmCIH m)

buildTopLevelHs ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  Module ()
buildTopLevelHs modname (mods, tmods) =
  mkModuleE modname pkgExtensions pkgExports pkgImports pkgBody
  where
    pkgExtensions =
      [ lang
          [ "FlexibleContexts",
            "FlexibleInstances",
            "ForeignFunctionInterface",
            "InterruptibleFFI"
          ]
      ]
    pkgExports =
      map (emodule . cmModule) mods
        ++ map emodule [modname <.> "Ordinary", modname <.> "Template", modname <.> "TH"]
    pkgImports = genImportInTopLevel modname (mods, tmods)
    pkgBody = [] --    map (genTopLevelFFI tih) (filterTLOrdinary tfns)
    -- ++ concatMap genTopLevelDef (filterTLOrdinary tfns)

buildTopLevelOrdinaryHs ::
  String ->
  ([ClassModule], [TemplateClassModule]) ->
  TopLevelImportHeader ->
  Module ()
buildTopLevelOrdinaryHs modname (_mods, tmods) tih =
  mkModuleE modname pkgExtensions pkgExports pkgImports pkgBody
  where
    tfns = tihFuncs tih
    pkgExtensions =
      [ lang
          [ "FlexibleContexts",
            "FlexibleInstances",
            "ForeignFunctionInterface",
            "InterruptibleFFI"
          ]
      ]
    pkgExports = map (evar . unqual . hsFrontNameForTopLevel . TLOrdinary) (filterTLOrdinary tfns)
    pkgImports =
      map mkImport ["Foreign.C", "Foreign.Ptr", "FFICXX.Runtime.Cast"]
        ++ map (\m -> mkImport (tcmModule m <.> "Template")) tmods
        ++ concatMap genImportForTLOrdinary (filterTLOrdinary tfns)
    pkgBody =
      map (genTopLevelFFI_ tih) (filterTLOrdinary tfns)
        ++ concatMap genTopLevelDef (filterTLOrdinary tfns)

buildTopLevelTemplateHs ::
  String ->
  TopLevelImportHeader ->
  Module ()
buildTopLevelTemplateHs modname tih =
  mkModuleE modname pkgExtensions pkgExports pkgImports pkgBody
  where
    tfns = filterTLTemplate (tihFuncs tih)
    pkgExtensions =
      [ lang
          [ "EmptyDataDecls",
            "FlexibleInstances",
            "ForeignFunctionInterface",
            "InterruptibleFFI",
            "MultiParamTypeClasses",
            "TypeFamilies"
          ]
      ]
    pkgExports =
      map
        ( (\n -> ethingwith (eWildCard 1) n [])
            . unqual
            . firstUpper
            . hsFrontNameForTopLevel
            . TLTemplate
        )
        tfns
    pkgImports =
      [ mkImport "Foreign.C.Types",
        mkImport "Foreign.Ptr",
        mkImport "FFICXX.Runtime.Cast"
      ]
        ++ concatMap genImportForTLTemplate tfns
    pkgBody = concatMap genTLTemplateInterface tfns

buildTopLevelTHHs ::
  String ->
  TopLevelImportHeader ->
  Module ()
buildTopLevelTHHs modname tih =
  mkModuleE modname pkgExtensions pkgExports pkgImports pkgBody
  where
    tfns = filterTLTemplate (tihFuncs tih)
    pkgExtensions =
      [ lang
          [ "FlexibleContexts",
            "FlexibleInstances",
            "ForeignFunctionInterface",
            "InterruptibleFFI",
            "TemplateHaskell"
          ]
      ]
    pkgExports =
      map
        ( evar
            . unqual
            . (\x -> "gen" <> x <> "InstanceFor")
            . firstUpper
            . hsFrontNameForTopLevel
            . TLTemplate
        )
        tfns
    pkgImports =
      [ mkImport "Data.Char",
        mkImport "Data.List",
        mkImport "Data.Monoid",
        mkImport "Foreign.C.Types",
        mkImport "Foreign.Ptr",
        mkImport "Language.Haskell.TH",
        mkImport "Language.Haskell.TH.Syntax",
        mkImport "FFICXX.Runtime.CodeGen.Cxx",
        mkImport "FFICXX.Runtime.TH"
      ]
        ++ concatMap genImportForTLTemplate tfns
    pkgBody =
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
