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
import qualified FFICXX.Generate.Util.HaskellSrcExts as O
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
import qualified Language.Haskell.Exts.Build as O (app, letE, name)
import qualified Language.Haskell.Exts.Syntax as O
  ( Decl,
    ImportDecl,
  )
import System.FilePath ((<.>))

mkImportWithDepCycles :: DepCycles -> String -> String -> O.ImportDecl ()
mkImportWithDepCycles depCycles self imported =
  let mloc = locateInDepCycles (self, imported) depCycles
   in case mloc of
        Just (idxSelf, idxImported)
          | idxImported > idxSelf ->
              O.mkImportSrc imported
        _ -> O.mkImport imported

genImportInInterface :: Bool -> DepCycles -> ClassModule -> [O.ImportDecl ()]
genImportInInterface isHsBoot depCycles m =
  let modSelf = cmModule m <.> "Interface"
      imported = cmImportedSubmodulesForInterface m
      (rdepsU, rdepsD) = getCyclicDepSubmodules modSelf depCycles
   in if isHsBoot
        then -- for hs-boot file, we ignore all module imports in the cycle.
        -- TODO: This is likely to be broken in more general cases.
        --       Keep improving this as hs-boot allows.

          let imported' = fmap subModuleName imported L.\\ (rdepsU <> rdepsD)
           in fmap O.mkImport imported'
        else fmap (mkImportWithDepCycles depCycles modSelf . subModuleName) imported

--
-- typeclass declaration
--

genHsFrontDecl :: Bool -> Class -> Reader AnnotateMap (O.Decl ())
genHsFrontDecl isHsBoot c = do
  -- TODO: revive annotation
  -- for the time being, let's ignore annotation.
  -- amap <- ask
  -- let cann = maybe "" id $ M.lookup (PkgClass,class_name c) amap
  let cdecl = O.mkClass (classConstraints c) (typeclassName c) [O.mkTBind "a"] body
      -- for hs-boot, we only have instance head.
      cdecl' = O.mkClass (O.cxTuple []) (typeclassName c) [O.mkTBind "a"] []
      sigdecl f = O.mkFunSig (hsFuncName c f) (functionSignature c f)
      body = map (O.clsDecl . sigdecl) . virtualFuncs . class_funcs $ c
  if isHsBoot
    then return cdecl'
    else return cdecl

------------
-- upcast --
------------

genHsFrontUpcastClass :: Class -> [O.Decl ()]
genHsFrontUpcastClass c = O.mkFun ("upcast" <> highname) typ [O.mkPVar "h"] rhs Nothing
  where
    (highname, rawname) = hsClassName c
    hightype = O.tycon highname
    rawtype = O.tycon rawname
    iname = typeclassName c
    a_bind = O.unkindedVar (O.name "a")
    a_tvar = O.mkTVar "a"
    typ =
      O.tyForall
        (Just [a_bind])
        (Just (O.cxTuple [O.classA (O.unqual "FPtr") [a_tvar], O.classA (O.unqual iname) [a_tvar]]))
        (O.tyfun a_tvar hightype)
    rhs =
      O.letE
        [ O.pbind (O.mkPVar "fh") (O.app (O.mkVar "get_fptr") (O.mkVar "h")) Nothing,
          O.pbind
            (O.mkPVarSig "fh2" (O.tyapp O.tyPtr rawtype))
            (O.app (O.mkVar "castPtr") (O.mkVar "fh"))
            Nothing
        ]
        (O.mkVar "cast_fptr_to_obj" `O.app` O.mkVar "fh2")

--------------
-- downcast --
--------------

genHsFrontDowncastClass :: Class -> [O.Decl ()]
genHsFrontDowncastClass c = O.mkFun ("downcast" <> highname) typ [O.mkPVar "h"] rhs Nothing
  where
    (highname, _rawname) = hsClassName c
    hightype = O.tycon highname
    iname = typeclassName c
    a_bind = O.unkindedVar (O.name "a")
    a_tvar = O.mkTVar "a"
    typ =
      O.tyForall
        (Just [a_bind])
        (Just (O.cxTuple [O.classA (O.unqual "FPtr") [a_tvar], O.classA (O.unqual iname) [a_tvar]]))
        (O.tyfun hightype a_tvar)
    rhs =
      O.letE
        [ O.pbind (O.mkPVar "fh") (O.app (O.mkVar "get_fptr") (O.mkVar "h")) Nothing,
          O.pbind (O.mkPVar "fh2") (O.app (O.mkVar "castPtr") (O.mkVar "fh")) Nothing
        ]
        (O.mkVar "cast_fptr_to_obj" `O.app` O.mkVar "fh2")
