module FFICXX.Generate.Code.HsCast where

import FFICXX.Generate.Name (hsClassName, typeclassName)
import FFICXX.Generate.Type.Class (Class (..), isAbstractClass)
import FFICXX.Generate.Util.GHCExactPrint
  ( app,
    classA,
    cxEmpty,
    cxTuple,
    instD,
    mkBind1,
    mkInstance,
    mkPVar,
    mkTVar,
    mkVar,
    tyPtr,
    tyapp,
    tycon,
  )
--

import qualified FFICXX.Generate.Util.HaskellSrcExts as O
  ( app,
    classA,
    cxEmpty,
    cxTuple,
    insDecl,
    mkBind1,
    mkInstance,
    mkPVar,
    mkTVar,
    mkVar,
    tyPtr,
    tyapp,
    tycon,
    unqual,
  )
import GHC.Hs (GhcPs)
import qualified Language.Haskell.Exts.Syntax as O (Decl, InstDecl)
import Language.Haskell.Syntax
  ( HsBind,
    HsDecl,
  )

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
      (app (mkVar "f") (app (mkVar "castPtr") (app (mkVar "get_fptr") (mkVar "x"))))
      Nothing,
    mkBind1
      "uncast"
      [mkPVar "x", mkPVar "f"]
      (app (mkVar "f") (app (mkVar "cast_fptr_to_obj") (app (mkVar "castPtr") (mkVar "x"))))
      Nothing
  ]

genHsFrontInstCastable :: Class -> Maybe (HsDecl GhcPs)
genHsFrontInstCastable c
  | (not . isAbstractClass) c =
      let iname = typeclassName c
          (_, rname) = hsClassName c
          a = mkTVar "a"
          ctxt = cxTuple [classA iname [a], classA "FPtr" [a]]
       in Just (instD (mkInstance ctxt "Castable" [a, tyapp tyPtr (tycon rname)] castBody))
  | otherwise = Nothing

genHsFrontInstCastableSelf :: Class -> Maybe (HsDecl GhcPs)
genHsFrontInstCastableSelf c
  | (not . isAbstractClass) c =
      let (cname, rname) = hsClassName c
       in Just (instD (mkInstance cxEmpty "Castable" [tycon cname, tyapp tyPtr (tycon rname)] castBody))
  | otherwise = Nothing
