{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsImplementation
  ( -- * import
    genImportInImplementation,

    -- * functions
    genHsFrontInst,
    genHsFrontInstNew,
    genHsFrontInstNonVirtual,
    genHsFrontInstStatic,
    genHsFrontInstVariables,
  )
where

import Control.Monad.Reader (Reader)
import FFICXX.Generate.Code.Primitive
  ( accessorSignature,
    convertCpp2HS,
    functionSignature,
    hsFuncXformer,
  )
import FFICXX.Generate.Name
  ( accessorName,
    aliasedFuncName,
    hsFuncName,
    hscAccessorName,
    hscFuncName,
    subModuleName,
    typeclassName,
  )
import FFICXX.Generate.Type.Annotate (AnnotateMap)
import FFICXX.Generate.Type.Class
  ( Accessor (..),
    Class (..),
    Types (..),
    isAbstractClass,
    isNewFunc,
    nonVirtualNotNewFuncs,
    staticFuncs,
    virtualFuncs,
  )
import FFICXX.Generate.Type.Module
  ( ClassModule (..),
  )
--
import FFICXX.Generate.Util.HaskellSrcExts
  ( cxEmpty,
    insDecl,
    mkBind1,
    mkFun,
    mkImport,
    mkInstance,
    mkVar,
  )
import Language.Haskell.Exts.Build (app)
import Language.Haskell.Exts.Syntax
  ( Decl,
    ImportDecl,
  )

--
-- import
--

genImportInImplementation :: ClassModule -> [ImportDecl ()]
genImportInImplementation m =
  fmap (mkImport . subModuleName) $ cmImportedSubmodulesForImplementation m

--
-- functions
--

genHsFrontInst :: Class -> Class -> [Decl ()]
genHsFrontInst parent child
  | (not . isAbstractClass) child =
      let idecl = mkInstance cxEmpty (typeclassName parent) [convertCpp2HS (Just child) SelfType] body
          defn f = mkBind1 (hsFuncName child f) [] rhs Nothing
            where
              rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName child f))
          body = map (insDecl . defn) . virtualFuncs . class_funcs $ parent
       in [idecl]
  | otherwise = []

genHsFrontInstNew ::
  -- | only concrete class
  Class ->
  Reader AnnotateMap [Decl ()]
genHsFrontInstNew c = do
  -- amap <- ask
  let fs = filter isNewFunc (class_funcs c)
  return . flip concatMap fs $ \f ->
    let -- for the time being, let's ignore annotation.
        -- cann = maybe "" id $ M.lookup (PkgMethod, constructorName c) amap
        -- newfuncann = mkComment 0 cann
        rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

genHsFrontInstNonVirtual :: Class -> [Decl ()]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing
  where
    nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

genHsFrontInstStatic :: Class -> [Decl ()]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = app (mkVar (hsFuncXformer f)) (mkVar (hscFuncName c f))
     in mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

genHsFrontInstVariables :: Class -> [Decl ()]
genHsFrontInstVariables c =
  flip concatMap (class_vars c) $ \v ->
    let rhs accessor =
          app
            (mkVar (case accessor of Getter -> "xform0"; _ -> "xform1"))
            (mkVar (hscAccessorName c v accessor))
     in mkFun (accessorName c v Getter) (accessorSignature c v Getter) [] (rhs Getter) Nothing
          <> mkFun (accessorName c v Setter) (accessorSignature c v Setter) [] (rhs Setter) Nothing
