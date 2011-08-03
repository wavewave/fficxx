{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, UndecidableInstances, 
             EmptyDataDecls, IncoherentInstances #-}


module HROOT.Class where

-- import Foreign.C            
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

import HROOT.Type
import HROOT.Function

$classBody$