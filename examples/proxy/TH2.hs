{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TH2 where

import FFICXX.Runtime.Cast
import Foreign.Ptr
import ProxyTest.Impl.Interface
import STD.Deletable.Interface
import qualified TH

TH.genImplProxy

data RawImplProxy

newtype ImplProxy = ImplProxy (Ptr RawImplProxy)
  deriving (Eq, Ord, Show)

instance () => FPtr (ImplProxy) where
  type Raw ImplProxy = RawImplProxy
  get_fptr (ImplProxy ptr) = ptr
  cast_fptr_to_obj = ImplProxy

instance () => Castable (ImplProxy) (Ptr RawImplProxy) where
  cast x f = f (castPtr (get_fptr x))
  uncast x f = f (cast_fptr_to_obj (castPtr x))

foreign import ccall safe "ImplProxy_delete"
  c_implsub_delete :: Ptr RawImplProxy -> IO ()

foreign import ccall safe "ImplProxy_newImplProxy"
  c_implsub_newimplsub :: Ptr () -> IO (Ptr RawImplProxy)

instance IDeletable ImplProxy where
  delete = xform0 c_implsub_delete

instance IImpl ImplProxy

newImplProxy :: Ptr () -> IO ImplProxy
newImplProxy = xform0 c_implsub_newimplsub
