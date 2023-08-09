module FFICXX.Generate.Code.HsRawType
  ( hsClassRawType,
  )
where

import FFICXX.Generate.Name (hsClassName)
import FFICXX.Generate.Type.Class (Class (..))
import FFICXX.Generate.Util.GHCExactPrint
  ( con,
    conDecl,
    cxEmpty,
    instD,
    mkBind1,
    mkData,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkVar,
    pApp,
    tyPtr,
    tyapp,
    tycon,
  )
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax
  ( HsDecl (TyClD),
    noExtField,
  )

{-
import FFICXX.Generate.Util.HaskellSrcExts
  ( con,
    conDecl,
    cxEmpty,
    ihcon,
    insDecl,
    insType,
    irule,
    mkBind1,
    mkDeriving,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkVar,
    qualConDecl,
    tyPtr,
    tyapp,
    tyfun,
    unqual,
  )
 import Language.Haskell.Exts.Build (name, pApp)
import Language.Haskell.Exts.Syntax
  ( Decl,
  )
-}

hsClassRawType :: Class -> [HsDecl GhcPs]
hsClassRawType c =
  [ TyClD noExtField (mkData rawname [] []), -- [] -- ,
    TyClD
      noExtField
      ( mkNewtype
          highname
          (conDecl highname [tyapp tyPtr rawtype])
          []
          {- [] [qualConDecl Nothing Nothing  mderiv -}
      ),
    instD $
      mkInstance
        cxEmpty
        "FPtr"
        [hightype]
        [ -- insType (tyapp (tycon "Raw") hightype) rawtype,
          mkBind1 "get_fptr" [pApp highname [mkPVar "ptr"]] (mkVar "ptr") Nothing,
          mkBind1 "cast_fptr_to_obj" [] (con highname) Nothing
        ]
  ]
  where
    (highname, rawname) = hsClassName c
    hightype = tycon highname
    rawtype = tycon rawname

{-    mderiv = Just (mkDeriving [i_eq, i_ord, i_show])
      where
        i_eq = irule Nothing Nothing (ihcon (unqual "Eq"))
        i_ord = irule Nothing Nothing (ihcon (unqual "Ord"))
        i_show = irule Nothing Nothing (ihcon (unqual "Show"))
-}
