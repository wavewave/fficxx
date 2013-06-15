-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.HsFFI
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------
module FFICXX.Generate.Code.HsFFI where

import Data.Char (toLower)
import System.FilePath ((<.>))
-- 
import FFICXX.Generate.Util 
import FFICXX.Generate.Type.Class


genHsFFI :: ClassImportHeader -> String 
genHsFFI header =
  let c = cihClass header
      h = cihSelfHeader header
      allfns = concatMap (virtualFuncs . class_funcs) 
                         (class_allparents c)
               ++ (class_funcs c) 
  in  intercalateWith connRet2 (hsFFIClassFunc h c) allfns  

genAllHsFFI :: [ClassImportHeader] -> String 
genAllHsFFI = intercalateWith connRet2 genHsFFI 

--------

-- | this template will be deprecated 
ffistub :: String
ffistub = "foreign import ccall \"$headerfilename$ $classname$_$funcname$\" $hsfuncname$ \n  :: $hsargs$"

-- | this template will be used.
ffiTemplate :: String
ffiTemplate = "foreign import ccall \"$headerfilename$ $funcname$\" $hsfuncname$ \n  :: $hsargs$"


hsFFIClassFunc :: FilePath -> Class -> Function -> String 
hsFFIClassFunc headerfilename c f = if isAbstractClass c 
                       then ""
                       else if (isNewFunc f || isStaticFunc f)
                              then render ffistub 
                                       [ ("headerfilename",headerfilename) 
                                       , ("classname",class_name c)
                                       , ("funcname", aliasedFuncName c f)
                                       , ("hsfuncname",hscFuncName c f)
                                       , ("hsargs", hsFuncTypNoSelf c f) ] 
                              else render ffistub 
                                       [ ("headerfilename",headerfilename) 
                                       , ("classname",class_name c)
                                       , ("funcname", aliasedFuncName c f)
                                       , ("hsfuncname",hscFuncName c f)
                                       , ("hsargs", hsFuncTyp c f) ] 

----------------------------
-- for top level function -- 
----------------------------

genTopLevelFuncFFI :: TopLevelImportHeader -> TopLevelFunction -> String 
genTopLevelFuncFFI header tfn = 
    let fname = maybe (toplevelfunc_name tfn) id (toplevelfunc_alias tfn)
        (x:xs)  = fname
        headerfilename = tihHeaderFileName header <.> "h"
        hfname = toLower x : xs 
        cfname = "c_" ++ toLowers hfname 
        args = toplevelfunc_args tfn 
        ret = toplevelfunc_ret tfn         
        argstr = concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret 
    in render ffiTemplate
         [ ("headerfilename",headerfilename) 
         , ("funcname", "TopLevel_" ++ fname)
         , ("hsfuncname",cfname)
         , ("hsargs", argstr) ] 
  where hsargtype (CT ctype _) = hsCTypeName ctype
        hsargtype (CPT x _) = hsCppTypeName x 
        hsargtype SelfType = "genTopLevelFuncFFI : no self for top level function " 
        hsargtype _ = error "undefined hsargtype"

        hsrettype Void = "IO ()"
        hsrettype SelfType = "genTopLevelFuncFFI : no self for top level function "
        hsrettype (CT ctype _) = "IO " ++ hsCTypeName ctype
        hsrettype (CPT x _ ) = "IO " ++ hsCppTypeName x 


