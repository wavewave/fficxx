{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.PackageInterface
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.PackageInterface where 

import           Data.Hashable
import qualified Data.HashMap.Strict as HM

newtype PackageName = PkgName String  deriving (Hashable, Show, Eq, Ord)
newtype ClassName = ClsName String deriving (Hashable, Show, Eq, Ord)
newtype HeaderName = HdrName String deriving (Hashable, Show, Eq, Ord)

type PackageInterface = HM.HashMap (PackageName, ClassName) HeaderName 

newtype TypeMacro = TypMcro { unTypMcro :: String } 
                  deriving (Show,Eq,Ord)
