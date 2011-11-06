{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, 
             EmptyDataDecls, ExistentialQuantification, ScopedTypeVariables #-}

$rawtypeHeader$

import Data.Word
import Foreign.C            
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import HROOT.TypeCast  

$rawtypeBody$