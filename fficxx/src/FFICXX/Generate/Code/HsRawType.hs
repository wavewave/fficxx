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
    mkBind1_,
    mkData,
    mkDeriving,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkTypeFamInst,
    mkVar,
    pApp,
    parP,
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
        [ mkBind1_
            "get_fptr"
            [parP (pApp highname [mkPVar "ptr"])]
            (mkVar "ptr"),
          mkBind1_ "cast_fptr_to_obj" [] (con highname)
        ]
  ]
  where
    (highname, rawname) = hsClassName c
    hightype = tycon highname
    rawtype = tycon rawname
    deriv = mkDeriving [tycon "Eq", tycon "Ord", tycon "Show"]
