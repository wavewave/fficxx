{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Generate.Type.PackageInterface where 

import Data.Hashable                       ( Hashable )
import qualified Data.HashMap.Strict as HM
--
import FFICXX.Runtime.CodeGen.C ( HeaderName(..), Namespace(..) )


newtype PackageName = PkgName String  deriving (Hashable, Show, Eq, Ord)
newtype ClassName = ClsName String deriving (Hashable, Show, Eq, Ord)



type PackageInterface = HM.HashMap (PackageName, ClassName) HeaderName 

newtype TypeMacro = TypMcro { unTypMcro :: String } 
                  deriving (Show,Eq,Ord)
