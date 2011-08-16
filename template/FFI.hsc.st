{-# LANGUAGE ForeignFunctionInterface #-}

module HROOT.Class.FFI where

import Foreign.C            
import Foreign.Ptr

import HROOT.Class.Interface

#include "$headerFileName$"

$hsFunctionBody$

