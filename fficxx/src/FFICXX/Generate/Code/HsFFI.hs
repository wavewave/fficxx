{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.HsFFI where

import Data.Maybe (fromMaybe, mapMaybe)
import FFICXX.Generate.Code.Primitive
  ( CFunSig (..),
    accessorCFunSig,
    genericFuncArgs,
    genericFuncRet,
    hsFFIFunType,
  )
--
import qualified FFICXX.Generate.Code.Primitive as O (hsFFIFuncTyp)
import FFICXX.Generate.Dependency
  ( class_allparents,
  )
import FFICXX.Generate.Name
  ( aliasedFuncName,
    ffiClassName,
    hscAccessorName,
    hscFuncName,
    subModuleName,
  )
import FFICXX.Generate.Type.Class
  ( Accessor (Getter, Setter),
    Arg (..),
    Class (..),
    Function (..),
    Selfness (NoSelf, Self),
    TLOrdinary (..),
    Variable (unVariable),
    isAbstractClass,
    isNewFunc,
    isStaticFunc,
    virtualFuncs,
  )
import FFICXX.Generate.Type.Module
  ( ClassImportHeader (..),
    ClassModule (..),
    TopLevelImportHeader (..),
  )
import FFICXX.Generate.Util (toLowers)
import FFICXX.Generate.Util.GHCExactPrint
  ( mkForImpCcall,
    mkImport,
  )
import qualified FFICXX.Generate.Util.HaskellSrcExts as O
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..))
import GHC.Hs
  ( GhcPs,
  )
import qualified Language.Haskell.Exts.Syntax as O
import Language.Haskell.Syntax
  ( ForeignDecl,
    ImportDecl,
  )
import System.FilePath ((<.>))

genHsFFI :: ClassImportHeader -> [ForeignDecl GhcPs]
genHsFFI header =
  let c = cihClass header
      -- TODO: This C header information should not be necessary according to up-to-date
      --       version of Haskell FFI.
      h = cihSelfHeader header
      -- NOTE: We need to generate FFI both for member functions at the current class level
      --       and parent level. For example, consider a class A with method foo, which a
      --       subclass of B with method bar. Then, A::foo (c_a_foo) and A::bar (c_a_bar)
      --       are made into a FFI function.
      allfns =
        concatMap
          (virtualFuncs . class_funcs)
          (class_allparents c)
          <> (class_funcs c)
   in mapMaybe (hsFFIClassFunc h c) allfns
        <> concatMap
          (\v -> [hsFFIAccessor c v Getter, hsFFIAccessor c v Setter])
          (class_vars c)

hsFFIClassFunc :: HeaderName -> Class -> Function -> Maybe (ForeignDecl GhcPs)
hsFFIClassFunc headerfilename c f =
  if isAbstractClass c
    then Nothing
    else
      let hfile = unHdrName headerfilename
          -- TODO: Make this a separate function
          cname = ffiClassName c <> "_" <> aliasedFuncName c f
          csig = CFunSig (genericFuncArgs f) (genericFuncRet f)
          typ =
            if (isNewFunc f || isStaticFunc f)
              then hsFFIFunType (Just (NoSelf, c)) csig
              else hsFFIFunType (Just (Self, c)) csig
       in Just (mkForImpCcall (hfile <> " " <> cname) (hscFuncName c f) typ)

hsFFIAccessor :: Class -> Variable -> Accessor -> ForeignDecl GhcPs
hsFFIAccessor c v a =
  let -- TODO: make this a separate function
      cname = ffiClassName c <> "_" <> arg_name (unVariable v) <> "_" <> (case a of Getter -> "get"; Setter -> "set")
      typ =
        hsFFIFunType
          (Just (Self, c))
          (accessorCFunSig (arg_type (unVariable v)) a)
   in mkForImpCcall cname (hscAccessorName c v a) typ

-- import for FFI
genImportInFFI :: ClassModule -> [ImportDecl GhcPs]
genImportInFFI = fmap (mkImport . subModuleName) . cmImportedSubmodulesForFFI

----------------------------
-- for top level function --
----------------------------

genTopLevelFFI :: TopLevelImportHeader -> TLOrdinary -> ForeignDecl GhcPs
genTopLevelFFI header tfn = mkForImpCcall (hfilename <> " TopLevel_" <> fname) cfname typ
  where
    (fname, args, ret) =
      case tfn of
        TopLevelFunction {..} -> (fromMaybe toplevelfunc_name toplevelfunc_alias, toplevelfunc_args, toplevelfunc_ret)
        TopLevelVariable {..} -> (fromMaybe toplevelvar_name toplevelvar_alias, [], toplevelvar_ret)
    hfilename = tihHeaderFileName header <.> "h"
    -- TODO: This must be exposed as a top-level function
    cfname = "c_" <> toLowers fname
    typ = hsFFIFunType Nothing (CFunSig args ret)

-- TODO: Remove
genTopLevelFFI_ :: TopLevelImportHeader -> TLOrdinary -> O.Decl ()
genTopLevelFFI_ header tfn = O.mkForImpCcall (hfilename <> " TopLevel_" <> fname) cfname typ
  where
    (fname, args, ret) =
      case tfn of
        TopLevelFunction {..} -> (fromMaybe toplevelfunc_name toplevelfunc_alias, toplevelfunc_args, toplevelfunc_ret)
        TopLevelVariable {..} -> (fromMaybe toplevelvar_name toplevelvar_alias, [], toplevelvar_ret)
    hfilename = tihHeaderFileName header <.> "h"
    -- TODO: This must be exposed as a top-level function
    cfname = "c_" <> toLowers fname
    typ = O.hsFFIFuncTyp Nothing (CFunSig args ret)
