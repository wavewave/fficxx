{-# LANGUAGE ForeignFunctionInterface #-}

module HROOT.Function where

import Foreign.C            
import Foreign.Ptr

import HROOT.Type

#include "$headerFileName$"

$hsFunctionBody$

