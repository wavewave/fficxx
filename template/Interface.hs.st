{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, 
             EmptyDataDecls, ExistentialQuantification, ScopedTypeVariables #-}

-- module HROOT.Class.Interface where

$ifaceHeader$

import Data.Word
import Foreign.C            
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import System.IO.Unsafe

import HROOT.TypeCast

$ifaceImport$

$ifaceBody$