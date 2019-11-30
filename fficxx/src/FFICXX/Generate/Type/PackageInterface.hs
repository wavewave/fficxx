{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: remove this module
module FFICXX.Generate.Type.PackageInterface where 

import Data.Hashable              ( Hashable )
import qualified Data.HashMap.Strict as HM
--
import FFICXX.Runtime.CodeGen.Cxx ( HeaderName(..) )


newtype PackageName = PkgName String  deriving (Hashable, Show, Eq, Ord)
newtype ClassName = ClsName String deriving (Hashable, Show, Eq, Ord)

type PackageInterface = HM.HashMap (PackageName, ClassName) HeaderName 
