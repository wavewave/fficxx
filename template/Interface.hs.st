{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, 
             EmptyDataDecls, ExistentialQuantification, ScopedTypeVariables #-}

-- module HROOT.Class.Interface where

$ifaceHeader$

import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import FFICXX.Runtime.Cast

$ifaceImport$

$ifaceBody$