{-# LANGUAGE ForeignFunctionInterface, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances,
  EmptyDataDecls, ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TH2 where

import Foreign.Ptr
import FFICXX.Runtime.Cast

import qualified TH

TH.genImplProxy

data RawImplSub

newtype ImplSub = ImplSub (Ptr RawImplSub)
                 deriving (Eq, Ord, Show)

instance () => FPtr (ImplSub) where
        type Raw ImplSub = RawImplSub
        get_fptr (ImplSub ptr) = ptr
        cast_fptr_to_obj = ImplSub

foreign import ccall safe "ImplSub_delete"
  c_impl_delete :: Ptr RawImplSub -> IO ()

-- foreign import ccall safe "ImplSub_newImpl"
--   c_impl_newimpl :: IO (Ptr RawImplSub)
