{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.HsFFI
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.HsFFI where

import           Data.Maybe                              ( fromMaybe, mapMaybe )
import           Data.Monoid                             ( (<>) )
import           Language.Haskell.Exts.Syntax            ( Type(..), Decl(..), unit_tycon)
import           System.FilePath ((<.>))
-- 
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface

genHsFFI :: ClassImportHeader -> [Decl]
genHsFFI header =
  let c = cihClass header
      h = cihSelfHeader header
      allfns = concatMap (virtualFuncs . class_funcs) 
                         (class_allparents c)
               <> (class_funcs c) 
  in mapMaybe (hsFFIClassFunc h c) allfns

--------

hsFFIClassFunc :: HeaderName -> Class -> Function -> Maybe Decl
hsFFIClassFunc headerfilename c f =
  if isAbstractClass c 
  then Nothing
  else let hfile = unHdrName headerfilename
           cname = class_name c <> "_" <> aliasedFuncName c f
           typ = if (isNewFunc f || isStaticFunc f)
                 then hsFFIFuncTyp NoSelf c f
                 else hsFFIFuncTyp Self c f
       in Just (mkForImpCcall (hfile <> " " <> cname) (hscFuncName c f) typ)
         
----------------------------
-- for top level function -- 
----------------------------

genTopLevelFuncFFI :: TopLevelImportHeader -> TopLevelFunction -> Decl
genTopLevelFuncFFI header tfn = mkForImpCcall (hfilename <> " TopLevel_" <> fname) cfname typ
  where (fname,args,ret) =
          case tfn of
            TopLevelFunction {..} -> (fromMaybe toplevelfunc_name toplevelfunc_alias, toplevelfunc_args, toplevelfunc_ret)
            TopLevelVariable {..} -> (fromMaybe toplevelvar_name toplevelvar_alias, [], toplevelvar_ret)
        hfilename = tihHeaderFileName header <.> "h"
        cfname = "c_" <> toLowers fname
        typ = foldr1 TyFun (map (hsargtype . fst) args <> [TyApp (tycon "IO") (hsrettype ret)])

        hsargtype (CT ctype _) = tycon (hsCTypeName ctype)
        hsargtype (CPT (CPTClass c) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName c)
        hsargtype (CPT (CPTClassRef c) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName c)
        hsargtype SelfType     = error "no self for top level function"
        hsargtype _ = error "undefined hsargtype"

        hsrettype Void         = unit_tycon
        hsrettype SelfType     = error "no self from top level function"
        hsrettype (CT ctype _) = tycon (hsCTypeName ctype)
        hsrettype (CPT (CPTClass c) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName c)
        hsrettype (CPT (CPTClassRef c) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName c)
        hsrettype (TemplateApp t p _) = TyApp tyPtr (TyApp (tycon rawname) (tycon p))
          where rawname = snd (hsTemplateClassName t)
        hsrettype (TemplateType t) = TyApp tyPtr (TyApp (tycon rawname) (mkTVar (tclass_param t)))
          where rawname = snd (hsTemplateClassName t)
        hsrettype (TemplateParam p) = mkTVar p

