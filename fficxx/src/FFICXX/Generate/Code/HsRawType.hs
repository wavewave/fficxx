module FFICXX.Generate.Code.HsRawType
  ( hsClassRawType,
  )
where

import FFICXX.Generate.Name (hsClassName)
import FFICXX.Generate.Type.Class (Class (..))
import FFICXX.Generate.Util.HaskellSrcExts
  ( con,
    conDecl,
    cxEmpty,
    ihcon,
    insDecl,
    insType,
    irule,
    mkBind1,
    mkData,
    mkDeriving,
    mkInstance,
    mkNewtype,
    mkPVar,
    mkVar,
    qualConDecl,
    tyPtr,
    tyapp,
    tycon,
    tyfun,
    unqual,
  )
import Language.Haskell.Exts.Build (name, pApp)
import Language.Haskell.Exts.Syntax
  ( Decl,
  )

hsClassRawType :: Class -> [Decl ()]
hsClassRawType c =
  [ mkData rawname [] [] Nothing,
    mkNewtype highname [] [qualConDecl Nothing Nothing (conDecl highname [tyapp tyPtr rawtype])] mderiv,
    mkInstance
      cxEmpty
      "FPtr"
      [hightype]
      [ insType (tyapp (tycon "Raw") hightype) rawtype,
        insDecl (mkBind1 "get_fptr" [pApp (name highname) [mkPVar "ptr"]] (mkVar "ptr") Nothing),
        insDecl (mkBind1 "cast_fptr_to_obj" [] (con highname) Nothing)
      ]
  ]
  where
    (highname, rawname) = hsClassName c
    hightype = tycon highname
    rawtype = tycon rawname
    mderiv = Just (mkDeriving [i_eq, i_ord, i_show])
      where
        i_eq = irule Nothing Nothing (ihcon (unqual "Eq"))
        i_ord = irule Nothing Nothing (ihcon (unqual "Ord"))
        i_show = irule Nothing Nothing (ihcon (unqual "Show"))
