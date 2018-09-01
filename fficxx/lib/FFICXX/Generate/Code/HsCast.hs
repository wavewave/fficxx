module FFICXX.Generate.Code.HsCast where

import Language.Haskell.Exts.Build             (app)
import Language.Haskell.Exts.Syntax            (Decl(..),InstDecl(..))
--
import FFICXX.Generate.Code.Primitive          (hsClassName,typeclassName)
import FFICXX.Generate.Type.Class              (Class(..),isAbstractClass)
import FFICXX.Generate.Util.HaskellSrcExts     (classA
                                               ,cxEmpty,cxTuple,insDecl
                                               ,mkBind1,mkInstance,mkPVar,mkTVar,mkVar
                                               ,tyapp,tycon,tyPtr
                                               ,unqual)
-----

castBody :: [InstDecl ()]
castBody =
  [ insDecl (mkBind1 "cast" [mkPVar "x",mkPVar "f"] (app (mkVar "f") (app (mkVar "castPtr") (app (mkVar "get_fptr") (mkVar "x")))) Nothing)
  , insDecl (mkBind1 "uncast" [mkPVar "x",mkPVar "f"] (app (mkVar "f") (app (mkVar "cast_fptr_to_obj") (app (mkVar "castPtr") (mkVar "x")))) Nothing)
  ]

genHsFrontInstCastable :: Class -> Maybe (Decl ())
genHsFrontInstCastable c
  | (not.isAbstractClass) c =
    let iname = typeclassName c
        (_,rname) = hsClassName c
        a = mkTVar "a"
        ctxt = cxTuple [ classA (unqual iname) [a], classA (unqual "FPtr") [a] ]
    in Just (mkInstance ctxt "Castable" [a,tyapp tyPtr (tycon rname)] castBody)
  | otherwise = Nothing

genHsFrontInstCastableSelf :: Class -> Maybe (Decl ())
genHsFrontInstCastableSelf c
  | (not.isAbstractClass) c =
    let (cname,rname) = hsClassName c
    in Just (mkInstance cxEmpty "Castable" [tycon cname, tyapp tyPtr (tycon rname)] castBody)
  | otherwise = Nothing

