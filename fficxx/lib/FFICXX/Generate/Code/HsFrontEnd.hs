{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.HsFrontEnd
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.HsFrontEnd where

import Control.Monad.Reader
import Data.Either                             (lefts,rights)
import Data.List
import Data.Monoid                             ((<>))
import Language.Haskell.Exts.Build             (app,letE,name,pApp)
import Language.Haskell.Exts.Syntax            (Decl(..),ExportSpec(..),ImportDecl(..))
import System.FilePath                         ((<.>))
--
import FFICXX.Generate.Code.Primitive          (HsFunSig(..)
                                               ,accessorSignature
                                               ,classConstraints
                                               ,convertCpp2HS
                                               ,extractArgRetTypes
                                               ,functionSignature
                                               ,hsFuncXformer
                                               )
import FFICXX.Generate.Name                    (accessorName
                                               ,aliasedFuncName
                                               ,constructorName
                                               ,hsClassName
                                               ,hscAccessorName
                                               ,hscFuncName
                                               ,hsFuncName
                                               ,hsFrontNameForTopLevelFunction
                                               ,typeclassName)
import FFICXX.Generate.Dependency              (class_allparents
                                               ,extractClassDepForTopLevelFunction
                                               ,getClassModuleBase,getTClassModuleBase
                                               ,argumentDependency,returnDependency
                                               )
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Annotate
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Util
import FFICXX.Generate.Util.HaskellSrcExts


genHsFrontDecl :: Class -> Reader AnnotateMap (Decl ())
genHsFrontDecl c = do
  -- TODO: revive annotation
  -- for the time being, let's ignore annotation.
  -- amap <- ask
  -- let cann = maybe "" id $ M.lookup (PkgClass,class_name c) amap
  let cdecl = mkClass (classConstraints c) (typeclassName c) [mkTBind "a"] body
      sigdecl f = mkFunSig (hsFuncName c f) (functionSignature c f)
      body = map (clsDecl . sigdecl) . virtualFuncs . class_funcs $ c
  return cdecl

-------------------

genHsFrontInst :: Class -> Class -> [Decl ()]
genHsFrontInst parent child
  | (not.isAbstractClass) child =
    let idecl = mkInstance cxEmpty (typeclassName parent) [convertCpp2HS (Just child) SelfType] body
        defn f = mkBind1 (hsFuncName child f) [] rhs Nothing
          where rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName child f))
        body = map (insDecl . defn) . virtualFuncs . class_funcs $ parent
    in [idecl]
  | otherwise = []




---------------------

genHsFrontInstNew :: Class         -- ^ only concrete class
                  -> Reader AnnotateMap [Decl ()]
genHsFrontInstNew c = do
  -- amap <- ask
  let fs = filter isNewFunc (class_funcs c)
  return . flip concatMap fs $ \f ->
    let
        -- for the time being, let's ignore annotation.
        -- cann = maybe "" id $ M.lookup (PkgMethod, constructorName c) amap
        -- newfuncann = mkComment 0 cann
        rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
    in mkFun (constructorName c) (functionSignature c f) [] rhs Nothing

genHsFrontInstNonVirtual :: Class -> [Decl ()]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
    in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing
 where nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

-----

genHsFrontInstStatic :: Class -> [Decl ()]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
    in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

-----

genHsFrontInstVariables :: Class -> [Decl ()]
genHsFrontInstVariables c =
  flip concatMap (class_vars c) $ \v ->
    let rhs accessor =
          app (mkVar (case accessor of Getter -> "xform0"; _ -> "xform1"))
              (mkVar (hscAccessorName c v accessor))
    in    mkFun (accessorName c v Getter) (accessorSignature c v Getter) [] (rhs Getter) Nothing
       <> mkFun (accessorName c v Setter) (accessorSignature c v Setter) [] (rhs Setter) Nothing




--------------------------

hsClassRawType :: Class -> [Decl ()]
hsClassRawType c =
  [ mkData    rawname [] [] Nothing
  , mkNewtype highname [] [qualConDecl Nothing Nothing (conDecl highname [tyapp tyPtr rawtype])] mderiv
  , mkInstance cxEmpty "FPtr" [hightype]
      [ insType (tyapp (tycon "Raw") hightype) rawtype
      , insDecl (mkBind1 "get_fptr" [pApp (name highname) [mkPVar "ptr"]] (mkVar "ptr") Nothing)
      , insDecl (mkBind1 "cast_fptr_to_obj" [] (con highname) Nothing)
      ]

  ]
 where (highname,rawname) = hsClassName c
       hightype = tycon highname
       rawtype = tycon rawname
       mderiv = Just (mkDeriving [i_eq,i_ord,i_show])
         where i_eq   = irule Nothing Nothing (ihcon (unqual "Eq"))
               i_ord  = irule Nothing Nothing (ihcon (unqual "Ord"))
               i_show = irule Nothing Nothing (ihcon (unqual "Show"))


------------
-- upcast --
------------

genHsFrontUpcastClass :: Class -> [Decl ()]
genHsFrontUpcastClass c = mkFun ("upcast"<>highname) typ [mkPVar "h"] rhs Nothing
  where (highname,rawname) = hsClassName c
        hightype = tycon highname
        rawtype = tycon rawname
        iname = typeclassName c
        a_bind = unkindedVar (name "a")
        a_tvar = mkTVar "a"
        typ = tyForall (Just [a_bind])
                (Just (cxTuple [classA (unqual "FPtr") [a_tvar], classA (unqual iname) [a_tvar]]))
                (tyfun a_tvar hightype)
        rhs = letE [ pbind (mkPVar "fh") (app (mkVar "get_fptr") (mkVar "h")) Nothing
                   , pbind (mkPVarSig "fh2" (tyapp tyPtr rawtype))
                       (app (mkVar "castPtr") (mkVar "fh")) Nothing
                   ]
                   (mkVar "cast_fptr_to_obj" `app` mkVar "fh2")


--------------
-- downcast --
--------------

genHsFrontDowncastClass :: Class -> [Decl ()]
genHsFrontDowncastClass c = mkFun ("downcast"<>highname) typ [mkPVar "h"] rhs Nothing
  where (highname,_rawname) = hsClassName c
        hightype = tycon highname
        iname = typeclassName c
        a_bind = unkindedVar (name "a")
        a_tvar = mkTVar "a"
        typ = tyForall (Just [a_bind])
                (Just (cxTuple [classA (unqual "FPtr") [a_tvar], classA (unqual iname) [a_tvar]]))
                (tyfun hightype a_tvar)
        rhs = letE [ pbind (mkPVar "fh") (app (mkVar "get_fptr") (mkVar "h")) Nothing
                   , pbind (mkPVar "fh2") (app (mkVar "castPtr") (mkVar "fh")) Nothing
                   ]
                   (mkVar "cast_fptr_to_obj" `app` mkVar "fh2")


------------------------
-- Top Level Function --
------------------------


genTopLevelFuncDef :: TopLevelFunction -> [Decl ()]
genTopLevelFuncDef f@TopLevelFunction {..} =
    let fname = hsFrontNameForTopLevelFunction f
        HsFunSig typs assts =
          extractArgRetTypes
            Nothing
            False
            (CFunSig toplevelfunc_args toplevelfunc_ret)
        sig = tyForall Nothing (Just (cxTuple assts)) (foldr1 tyfun typs)
        xformerstr = let len = length toplevelfunc_args in if len > 0 then "xform" <> show (len-1) else "xformnull"
        cfname = "c_" <> toLowers fname
        rhs = app (mkVar xformerstr) (mkVar cfname)

    in mkFun fname sig [] rhs Nothing
genTopLevelFuncDef v@TopLevelVariable {..} =
    let fname = hsFrontNameForTopLevelFunction v
        cfname = "c_" <> toLowers fname
        rtyp = convertCpp2HS Nothing toplevelvar_ret
        sig = tyapp (tycon "IO") rtyp
        rhs = app (mkVar "xformnull") (mkVar cfname)

    in mkFun fname sig [] rhs Nothing


------------
-- Export --
------------

genExport :: Class -> [ExportSpec ()]
genExport c =
    let espec n = if null . (filter isVirtualFunc) $ (class_funcs c)
                    then eabs nonamespace (unqual n)
                    else ethingall (unqual n)
    in if isAbstractClass c
       then [ espec (typeclassName c) ]
       else [ ethingall (unqual ((fst.hsClassName) c))
            , espec (typeclassName c)
            , evar (unqual ("upcast" <> (fst.hsClassName) c))
            , evar (unqual ("downcast" <> (fst.hsClassName) c)) ]
            <> genExportConstructorAndNonvirtual c
            <> genExportStatic c

-- | constructor and non-virtual function
genExportConstructorAndNonvirtual :: Class -> [ExportSpec ()]
genExportConstructorAndNonvirtual c = map (evar . unqual) fns
  where fs = class_funcs c
        fns = map (aliasedFuncName c) (constructorFuncs fs
                                       <> nonVirtualNotNewFuncs fs)

-- | staic function export list
genExportStatic :: Class -> [ExportSpec ()]
genExportStatic c = map (evar . unqual) fns
  where fs = class_funcs c
        fns = map (aliasedFuncName c) (staticFuncs fs)


------------
-- Import --
------------

genExtraImport :: ClassModule -> [ImportDecl ()]
genExtraImport cm = map mkImport (cmExtraImport cm)


genImportInModule :: [Class] -> [ImportDecl ()]
genImportInModule = concatMap (\x -> map (\y -> mkImport (getClassModuleBase x<.>y)) ["RawType","Interface","Implementation"])



genImportInInterface :: ClassModule -> [ImportDecl ()]
genImportInInterface m =
  let modlstraw = cmImportedModulesRaw m
      modlstparent = cmImportedModulesHighNonSource m
      modlsthigh = cmImportedModulesHighSource m
  in  [mkImport (cmModule m <.> "RawType")]
      <> flip map modlstraw
           (\case
               Left t -> mkImport (getTClassModuleBase t <.> "Template")
               Right c -> mkImport (getClassModuleBase c <.>"RawType")
           )
      <> flip map modlstparent
           (\case
               Left t -> mkImport (getTClassModuleBase t <.> "Template")
               Right c -> mkImport (getClassModuleBase c <.>"Interface")
           )
      <> flip map modlsthigh
           (\case
               Left t  -> -- TODO: *.Template in the same package needs to have hs-boot.
                          --       Currently, we do not have it yet.
                          mkImport (getTClassModuleBase t <.> "Template")
               Right c -> mkImportSrc (getClassModuleBase c<.>"Interface")
           )

-- |
genImportInCast :: ClassModule -> [ImportDecl ()]
genImportInCast m = [ mkImport (cmModule m <.> "RawType")
                   ,  mkImport (cmModule m <.> "Interface") ]

-- |
genImportInImplementation :: ClassModule -> [ImportDecl ()]
genImportInImplementation m =
  let modlstraw' = cmImportedModulesForFFI m
      modlsthigh = nub $ map Right $ concatMap class_allparents (cmClass m)
      modlstraw = filter (not.(flip elem modlsthigh)) modlstraw'
  in  [ mkImport (cmModule m <.> "RawType")
      , mkImport (cmModule m <.> "FFI")
      , mkImport (cmModule m <.> "Interface")
      , mkImport (cmModule m <.> "Cast") ]
      <> concatMap (\case Left t -> [mkImport (getTClassModuleBase t <.> "Template")]; Right c -> map (\y -> mkImport (getClassModuleBase c<.>y)) ["RawType","Cast","Interface"]) modlstraw
      <> concatMap (\case Left t -> [mkImport (getTClassModuleBase t <.> "Template")]; Right c -> map (\y -> mkImport (getClassModuleBase c<.>y)) ["RawType","Cast","Interface"]) modlsthigh


-- | generate import list for a given top-level function
--   currently this may generate duplicate import list.
-- TODO: eliminate duplicated imports.
genImportForTopLevelFunction :: TopLevelFunction -> [ImportDecl ()]
genImportForTopLevelFunction f =
  let dep4func = extractClassDepForTopLevelFunction f
      ecs = returnDependency dep4func ++ argumentDependency dep4func
      cmods = nub $ map getClassModuleBase $ rights ecs
      tmods = nub $ map getTClassModuleBase $ lefts ecs
  in    concatMap (\x -> map (\y -> mkImport (x<.>y)) ["RawType","Cast","Interface"]) cmods
     <> concatMap (\x -> map (\y -> mkImport (x<.>y)) ["Template"]) tmods

-- | generate import list for top level module
genImportInTopLevel ::
     String
  -> ([ClassModule],[TemplateClassModule])
  -> TopLevelImportHeader
  -> [ImportDecl ()]
genImportInTopLevel modname (mods,tmods) tih =
  let tfns = tihFuncs tih
  in    map (mkImport . cmModule) mods
     ++ if null tfns
        then []
        else    map mkImport [ "Foreign.C", "Foreign.Ptr", "FFICXX.Runtime.Cast" ]
             ++ map (\c -> mkImport (modname <.> (fst.hsClassName.cihClass) c <.> "RawType")) (tihClassDep tih)
             ++ map (\m -> mkImport (tcmModule m <.> "Template")) tmods
             ++ concatMap genImportForTopLevelFunction tfns

