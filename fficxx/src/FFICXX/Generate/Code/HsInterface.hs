{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsInterface
  ( -- * import
    genImportInInterface,

    -- * typeclass
    genHsFrontDecl,

    -- * up/downcast
    genHsFrontUpcastClass,
    genHsFrontDowncastClass,
  )
where

import Control.Monad.Reader (Reader)
import qualified Data.List as L
import FFICXX.Generate.Code.Primitive
  ( classConstraints,
    functionSignature,
  )
import FFICXX.Generate.Dependency.Graph
  ( getCyclicDepSubmodules,
    locateInDepCycles,
  )
import FFICXX.Generate.Name
  ( hsClassName,
    hsFuncName,
    subModuleName,
    typeclassName,
  )
import FFICXX.Generate.Type.Annotate (AnnotateMap)
import FFICXX.Generate.Type.Class
  ( Class (..),
    virtualFuncs,
  )
import FFICXX.Generate.Type.Module
  ( ClassModule (..),
    DepCycles,
  )
import FFICXX.Generate.Util.HaskellSrcExts
  ( classA,
    clsDecl,
    cxTuple,
    mkClass,
    mkFun,
    mkFunSig,
    mkImport,
    mkImportSrc,
    mkPVar,
    mkPVarSig,
    mkTBind,
    mkTVar,
    mkVar,
    pbind,
    tyForall,
    tyPtr,
    tyapp,
    tycon,
    tyfun,
    unkindedVar,
    unqual,
  )
import Language.Haskell.Exts.Build (app, letE, name)
import Language.Haskell.Exts.Syntax
  ( Decl,
    ImportDecl,
  )
import System.FilePath ((<.>))

mkImportWithDepCycles :: DepCycles -> String -> String -> ImportDecl ()
mkImportWithDepCycles depCycles self imported =
  let mloc = locateInDepCycles (self, imported) depCycles
   in case mloc of
        Just (idxSelf, idxImported)
          | idxImported > idxSelf ->
              mkImportSrc imported
        _ -> mkImport imported

genImportInInterface :: Bool -> DepCycles -> ClassModule -> [ImportDecl ()]
genImportInInterface isHsBoot depCycles m =
  let modSelf = cmModule m <.> "Interface"
      imported = cmImportedSubmodulesForInterface m
      (rdepsU, rdepsD) = getCyclicDepSubmodules modSelf depCycles
   in if isHsBoot
        then -- for hs-boot file, we ignore all module imports in the cycle.
        -- TODO: This is likely to be broken in more general cases.
        --       Keep improving this as hs-boot allows.

          let imported' = fmap subModuleName imported L.\\ (rdepsU <> rdepsD)
           in fmap mkImport imported'
        else fmap (mkImportWithDepCycles depCycles modSelf . subModuleName) imported

--
-- typeclass declaration
--

genHsFrontDecl :: Bool -> Class -> Reader AnnotateMap (Decl ())
genHsFrontDecl isHsBoot c = do
  -- TODO: revive annotation
  -- for the time being, let's ignore annotation.
  -- amap <- ask
  -- let cann = maybe "" id $ M.lookup (PkgClass,class_name c) amap
  let cdecl = mkClass (classConstraints c) (typeclassName c) [mkTBind "a"] body
      -- for hs-boot, we only have instance head.
      cdecl' = mkClass (cxTuple []) (typeclassName c) [mkTBind "a"] []
      sigdecl f = mkFunSig (hsFuncName c f) (functionSignature c f)
      body = map (clsDecl . sigdecl) . virtualFuncs . class_funcs $ c
  if isHsBoot
    then return cdecl'
    else return cdecl

------------
-- upcast --
------------

genHsFrontUpcastClass :: Class -> [Decl ()]
genHsFrontUpcastClass c = mkFun ("upcast" <> highname) typ [mkPVar "h"] rhs Nothing
  where
    (highname, rawname) = hsClassName c
    hightype = tycon highname
    rawtype = tycon rawname
    iname = typeclassName c
    a_bind = unkindedVar (name "a")
    a_tvar = mkTVar "a"
    typ =
      tyForall
        (Just [a_bind])
        (Just (cxTuple [classA (unqual "FPtr") [a_tvar], classA (unqual iname) [a_tvar]]))
        (tyfun a_tvar hightype)
    rhs =
      letE
        [ pbind (mkPVar "fh") (app (mkVar "get_fptr") (mkVar "h")) Nothing,
          pbind
            (mkPVarSig "fh2" (tyapp tyPtr rawtype))
            (app (mkVar "castPtr") (mkVar "fh"))
            Nothing
        ]
        (mkVar "cast_fptr_to_obj" `app` mkVar "fh2")

--------------
-- downcast --
--------------

genHsFrontDowncastClass :: Class -> [Decl ()]
genHsFrontDowncastClass c = mkFun ("downcast" <> highname) typ [mkPVar "h"] rhs Nothing
  where
    (highname, _rawname) = hsClassName c
    hightype = tycon highname
    iname = typeclassName c
    a_bind = unkindedVar (name "a")
    a_tvar = mkTVar "a"
    typ =
      tyForall
        (Just [a_bind])
        (Just (cxTuple [classA (unqual "FPtr") [a_tvar], classA (unqual iname) [a_tvar]]))
        (tyfun hightype a_tvar)
    rhs =
      letE
        [ pbind (mkPVar "fh") (app (mkVar "get_fptr") (mkVar "h")) Nothing,
          pbind (mkPVar "fh2") (app (mkVar "castPtr") (mkVar "fh")) Nothing
        ]
        (mkVar "cast_fptr_to_obj" `app` mkVar "fh2")
