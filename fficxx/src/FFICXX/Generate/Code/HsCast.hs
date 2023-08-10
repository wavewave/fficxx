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
    mkBind1,
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
castBody_ :: [O.InstDecl ()]
castBody_ =
  [ O.insDecl (O.mkBind1 "cast" [O.mkPVar "x", O.mkPVar "f"] (O.app (O.mkVar "f") (O.app (O.mkVar "castPtr") (O.app (O.mkVar "get_fptr") (O.mkVar "x")))) Nothing),
    O.insDecl (O.mkBind1 "uncast" [O.mkPVar "x", O.mkPVar "f"] (O.app (O.mkVar "f") (O.app (O.mkVar "cast_fptr_to_obj") (O.app (O.mkVar "castPtr") (O.mkVar "x")))) Nothing)
  ]

castBody :: [HsBind GhcPs]
castBody =
  [ mkBind1
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
      )
      Nothing,
    mkBind1
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
      Nothing
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
