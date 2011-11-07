{-# LANGUAGE ForeignFunctionInterface #-}

-- module HROOT.Class.FFI where

$ffiHeader$

import Foreign.C            
import Foreign.Ptr

-- import HROOT.Class.Interface

-- #include "$headerFileName$"

$ffiImport$

$hsInclude$

$cppInclude$

$hsFunctionBody$

