{-# LANGUAGE ForeignFunctionInterface #-}

module HROOT.Class where

import Foreign.C            
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

import HROOT.Type

$classBody$