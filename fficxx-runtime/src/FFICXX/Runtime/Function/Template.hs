{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FFICXX.Runtime.Function.Template where

import FFICXX.Runtime.Cast
  ( Castable (..),
    FPtr (..),
    Raw (..),
  )
import Foreign.Ptr (FunPtr, Ptr, castPtr)

data RawFunction t

newtype Function t = Function (Ptr (RawFunction t))

class IFunction t where
  newFunction :: FunPtr t -> IO (Function t)
  call :: Function t -> t
  deleteFunction :: Function t -> IO ()

instance () => FPtr (Function t) where
  type Raw (Function t) = RawFunction t
  get_fptr (Function ptr) = ptr
  cast_fptr_to_obj = Function

instance () => Castable (Function t) (Ptr (RawFunction t)) where
  cast x f = f (castPtr (get_fptr x))
  uncast x f = f (cast_fptr_to_obj (castPtr x))

instance () => Castable (FunPtr t) (FunPtr t) where
  cast x f = f x
  uncast x f = f x

class FunPtrWrapper t where
  wrapFunPtr :: t -> IO (FunPtr t)
