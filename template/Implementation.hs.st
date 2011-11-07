{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, EmptyDataDecls, 
             OverlappingInstances, IncoherentInstances #-}

$implHeader$

import HROOT.TypeCast

$implImport$

import Data.Word
-- import Foreign.C            
-- import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe


$implBody$