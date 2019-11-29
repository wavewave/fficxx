{-# LANGUAGE ForeignFunctionInterface, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances,
  EmptyDataDecls, ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TH2 where

import Foreign.Ptr
import FFICXX.Runtime.Cast
import STD.Deletable.Interface
import InheritTest.Impl.Interface

import qualified TH

TH.genImplProxy

data RawImplSub

newtype ImplSub = ImplSub (Ptr RawImplSub)
                 deriving (Eq, Ord, Show)

instance () => FPtr (ImplSub) where
        type Raw ImplSub = RawImplSub
        get_fptr (ImplSub ptr) = ptr
        cast_fptr_to_obj = ImplSub

instance () => Castable (ImplSub) (Ptr RawImplSub) where
        cast x f = f (castPtr (get_fptr x))
        uncast x f = f (cast_fptr_to_obj (castPtr x))

foreign import ccall safe "ImplSub_delete"
  c_implsub_delete :: Ptr RawImplSub -> IO ()

foreign import ccall safe "ImplSub_newImplSub"
  c_implsub_newimplsub :: Ptr () -> IO (Ptr RawImplSub)

instance IDeletable ImplSub where
  delete = xform0 c_implsub_delete

instance IImpl ImplSub

newImplSub :: Ptr () -> IO ImplSub
newImplSub = xform0 c_implsub_newimplsub
