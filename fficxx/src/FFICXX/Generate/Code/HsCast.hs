module FFICXX.Generate.Code.HsCast
  ( -- * imports
    genImportInCast,

    -- * code
    castBody,
    castBody_,
    genHsFrontInstCastable,
    genHsFrontInstCastableSelf,
  )
where

import FFICXX.Generate.Name
  ( hsClassName,
    subModuleName,
    typeclassName,
  )
import FFICXX.Generate.Type.Class (Class (..), isAbstractClass)
import FFICXX.Generate.Type.Module (ClassModule (..))
import FFICXX.Generate.Util.GHCExactPrint
  ( app,
    classA,
    cxEmpty,
    cxTuple,
    instD,
    mkBind1_,
    mkImport,
    mkInstance,
    mkPVar,
    mkTVar,
    mkVar,
    par,
    tyPtr,
    tyapp,
    tycon,
  )
import qualified FFICXX.Generate.Util.HaskellSrcExts as O
  ( app,
    insDecl,
    mkBind1,
    mkPVar,
    mkVar,
  )
import GHC.Hs (GhcPs)
import qualified Language.Haskell.Exts.Syntax as O (InstDecl)
import Language.Haskell.Syntax
  ( HsBind,
    HsDecl,
    ImportDecl,
  )

--
-- imports
--

genImportInCast :: ClassModule -> [ImportDecl GhcPs]
genImportInCast m =
  fmap (mkImport . subModuleName) $ cmImportedSubmodulesForCast m

--
-- code
--

castBody :: [HsBind GhcPs]
castBody =
  [ mkBind1_
      "cast"
      [mkPVar "x", mkPVar "f"]
      ( app
          (mkVar "f")
          ( par
              ( app
                  (mkVar "castPtr")
                  ( par
                      ( app
                          (mkVar "get_fptr")
                          (mkVar "x")
                      )
                  )
              )
          )
      ),
    mkBind1_
      "uncast"
      [mkPVar "x", mkPVar "f"]
      ( app
          (mkVar "f")
          ( par
              ( app
                  (mkVar "cast_fptr_to_obj")
                  ( par
                      ( app
                          (mkVar "castPtr")
                          (mkVar "x")
                      )
                  )
              )
          )
      )
  ]

genHsFrontInstCastable :: Class -> Maybe (HsDecl GhcPs)
genHsFrontInstCastable c
  | (not . isAbstractClass) c =
      let iname = typeclassName c
          (_, rname) = hsClassName c
          a = mkTVar "a"
          ctxt = cxTuple [classA iname [a], classA "FPtr" [a]]
       in Just (instD (mkInstance ctxt "Castable" [a, tyapp tyPtr (tycon rname)] [] castBody))
  | otherwise = Nothing

genHsFrontInstCastableSelf :: Class -> Maybe (HsDecl GhcPs)
genHsFrontInstCastableSelf c
  | (not . isAbstractClass) c =
      let (cname, rname) = hsClassName c
       in Just $
            instD
              ( mkInstance
                  cxEmpty
                  "Castable"
                  [tycon cname, tyapp tyPtr (tycon rname)]
                  []
                  castBody
              )
  | otherwise = Nothing
