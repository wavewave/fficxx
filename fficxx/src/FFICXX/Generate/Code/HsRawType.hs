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
    mkDeriving,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkTypeFamInst,
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

hsClassRawType :: Class -> [HsDecl GhcPs]
hsClassRawType c =
  [ TyClD noExtField (mkData rawname [] []),
    TyClD
      noExtField
      ( mkNewtype
          highname
          (conDecl highname [tyapp tyPtr rawtype])
          deriv
      ),
    instD $
      mkInstance
        cxEmpty
        "FPtr"
        [hightype]
        [ mkTypeFamInst "Raw" [hightype] rawtype
        ]
        [ mkBind1 "get_fptr" [pApp highname [mkPVar "ptr"]] (mkVar "ptr") Nothing,
          mkBind1 "cast_fptr_to_obj" [] (con highname) Nothing
        ]
  ]
  where
    (highname, rawname) = hsClassName c
    hightype = tycon highname
    rawtype = tycon rawname
    deriv = mkDeriving [tycon "Eq", tycon "Ord", tycon "Show"]
