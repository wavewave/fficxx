{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, UndecidableInstances, 
             EmptyDataDecls, IncoherentInstances #-}


module HROOT.Class.Implementation where

import Data.Word
-- import Foreign.C            
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

import HROOT.Class.Interface
import HROOT.Class.FFI

$implBody$