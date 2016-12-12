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

import           Data.Char                               ( toLower )
import           Data.List
import           Data.Maybe                              ( fromMaybe, mapMaybe )
import           Data.Text                               ( Text )
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Text.Template                      hiding (render)
import           Language.Haskell.Exts.Syntax            ( Type(..), Exp(..), Decl(..)
                                                         , ClassDecl(..), InstDecl(..)
                                                         , Pat(..), Name(..), QOp(..), Op(..)
                                                         , Asst(..), ConDecl(..), QualConDecl(..)
                                                         , DataOrNew(..), TyVarBind (..), Binds(..)
                                                         , Rhs(..)
                                                         , unit_tycon)
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc            ( noLoc )
import           System.FilePath ((<.>))
-- 
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.PackageInterface

genHsFFI :: ClassImportHeader -> String 
genHsFFI header =
  let c = cihClass header
      h = cihSelfHeader header
      allfns = concatMap (virtualFuncs . class_funcs) 
                         (class_allparents c)
               ++ (class_funcs c) 
  in  intercalate "\n\n" $ map prettyPrint (mapMaybe (hsFFIClassFunc h c) allfns )

genAllHsFFI :: [ClassImportHeader] -> String 
genAllHsFFI = intercalateWith connRet2 genHsFFI 

--------


-- ffistub :: Text
-- ffistub = "foreign import ccall \"$headerfilename ${classname}_${funcname}\" $hsfuncname \n  :: $hsargs"

-- | this template will be used.
ffiTemplate :: Text
ffiTemplate = "foreign import ccall \"$headerfilename $funcname\" $hsfuncname \n  :: $hsargs"


hsFFIClassFunc :: HeaderName -> Class -> Function -> Maybe Decl
hsFFIClassFunc headerfilename c f =
  if isAbstractClass c 
  then Nothing
  else if (isNewFunc f || isStaticFunc f)
       then let hfile = unHdrName headerfilename
                cname = class_name c ++ "_" ++ aliasedFuncName c f
                typ = hsFuncTypNoSelf c f
            in Just (mkForImpCcall (hfile ++ " " ++ cname) (hscFuncName c f) typ)
       else let hfile = unHdrName headerfilename
                cname = class_name c ++ "_" ++ aliasedFuncName c f
                typ = hsFuncTyp c f
            in Just (mkForImpCcall (hfile ++ " " ++ cname) (hscFuncName c f) typ)
         
----------------------------
-- for top level function -- 
----------------------------

genTopLevelFuncFFI :: TopLevelImportHeader -> TopLevelFunction -> Decl
genTopLevelFuncFFI header tfn = mkForImpCcall (hfilename ++ " " ++ cfname) ("TopLevel_" ++ fname) typ
{-
  
    case tfn of
      TopLevelFunction {..} ->  
	    args = toplevelfunc_args 
	    ret = toplevelfunc_ret         
	    argstr = concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret 
        in subst ffiTemplate (context [ ("headerfilename", headerfilename      ) 
                                      , ("funcname"      , "TopLevel_" ++ fname)
                                      , ("hsfuncname"    , cfname              )
                                      , ("hsargs"        , argstr              ) ]) 
      TopLevelVariable {..} ->  
        let fname = maybe toplevelvar_name id toplevelvar_alias
	    args = [] 
            ret = toplevelvar_ret         
            argstr = concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret 
        in subst ffiTemplate (context [ ("headerfilename", headerfilename      ) 
                                      , ("funcname"      , "TopLevel_" ++ fname)
                                      , ("hsfuncname"    , cfname              )
                                      , ("hsargs"        , argstr              ) ]) 
-}
  where (fname,args,ret) =
          case tfn of
            TopLevelFunction {..} -> (fromMaybe toplevelfunc_name toplevelfunc_alias, toplevelfunc_args, toplevelfunc_ret)
            TopLevelVariable {..} -> (fromMaybe toplevelvar_name toplevelvar_alias, [], toplevelvar_ret)
        (x:xs)  = fname
        hfilename = tihHeaderFileName header <.> "h"
        -- hfname = toLower x : xs 
        cfname = "c_" ++ toLowers fname
        typ = foldl1 TyFun (map (hsargtype . fst) args ++ [TyApp (tycon "IO") (hsrettype ret)])

        hsargtype (CT ctype _) = tycon (hsCTypeName ctype)
        hsargtype (CPT x _)    = tycon (hsCppTypeName x)
        hsargtype SelfType     = error "no self for top level function"
        hsargtype _ = error "undefined hsargtype"

        hsrettype Void         = unit_tycon
        hsrettype SelfType     = error "no self fro top level function"
        hsrettype (CT ctype _) = tycon (hsCTypeName ctype)
        hsrettype (CPT x _ )   = tycon (hsCppTypeName x)


