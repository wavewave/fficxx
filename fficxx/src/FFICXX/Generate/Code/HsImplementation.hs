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
import qualified FFICXX.Generate.Util.HaskellSrcExts as O
  ( cxEmpty,
    insDecl,
    mkBind1,
    mkFun,
    mkImport,
    mkInstance,
    mkVar,
  )
import qualified Language.Haskell.Exts.Build as O (app)
import qualified Language.Haskell.Exts.Syntax as O
  ( Decl,
    ImportDecl,
  )

--
-- import
--

genImportInImplementation :: ClassModule -> [O.ImportDecl ()]
genImportInImplementation m =
  fmap (O.mkImport . subModuleName) $ cmImportedSubmodulesForImplementation m

--
-- functions
--

genHsFrontInst :: Class -> Class -> [O.Decl ()]
genHsFrontInst parent child
  | (not . isAbstractClass) child =
      let idecl = O.mkInstance O.cxEmpty (typeclassName parent) [convertCpp2HS (Just child) SelfType] body
          defn f = O.mkBind1 (hsFuncName child f) [] rhs Nothing
            where
              rhs = O.app (O.mkVar (hsFuncXformer f)) (O.mkVar (hscFuncName child f))
          body = map (O.insDecl . defn) . virtualFuncs . class_funcs $ parent
       in [idecl]
  | otherwise = []

genHsFrontInstNew ::
  -- | only concrete class
  Class ->
  Reader AnnotateMap [O.Decl ()]
genHsFrontInstNew c = do
  -- amap <- ask
  let fs = filter isNewFunc (class_funcs c)
  return . flip concatMap fs $ \f ->
    let -- for the time being, let's ignore annotation.
        -- cann = maybe "" id $ M.lookup (PkgMethod, constructorName c) amap
        -- newfuncann = mkComment 0 cann
        rhs = O.app (O.mkVar (hsFuncXformer f)) (O.mkVar (hscFuncName c f))
     in O.mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

genHsFrontInstNonVirtual :: Class -> [O.Decl ()]
genHsFrontInstNonVirtual c =
  flip concatMap nonvirtualFuncs $ \f ->
    let rhs = O.app (O.mkVar (hsFuncXformer f)) (O.mkVar (hscFuncName c f))
     in O.mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing
  where
    nonvirtualFuncs = nonVirtualNotNewFuncs (class_funcs c)

genHsFrontInstStatic :: Class -> [O.Decl ()]
genHsFrontInstStatic c =
  flip concatMap (staticFuncs (class_funcs c)) $ \f ->
    let rhs = O.app (O.mkVar (hsFuncXformer f)) (O.mkVar (hscFuncName c f))
     in O.mkFun (aliasedFuncName c f) (functionSignature c f) [] rhs Nothing

genHsFrontInstVariables :: Class -> [O.Decl ()]
genHsFrontInstVariables c =
  flip concatMap (class_vars c) $ \v ->
    let rhs accessor =
          O.app
            (O.mkVar (case accessor of Getter -> "xform0"; _ -> "xform1"))
            (O.mkVar (hscAccessorName c v accessor))
     in O.mkFun (accessorName c v Getter) (accessorSignature c v Getter) [] (rhs Getter) Nothing
          <> O.mkFun (accessorName c v Setter) (accessorSignature c v Setter) [] (rhs Setter) Nothing
