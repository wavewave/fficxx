{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies, 
             MultiParamTypeClasses, OverlappingInstances, IncoherentInstances #-}

$castHeader$

import Foreign.Ptr
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr_)
import Foreign.ForeignPtr.Unsafe
import FFICXX.Runtime.Cast
import System.IO.Unsafe

$castImport$

$castBody$
