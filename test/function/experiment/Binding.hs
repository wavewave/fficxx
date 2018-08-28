{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Binding where

-- import Foreign.C.Types
import Foreign.Ptr

data RawFunction t

newtype Function t = Function (Ptr (RawFunction t))

foreign import ccall safe "Function_new_f1"
   c_Function_new_f1 :: FunPtr (IO ()) -> IO (Function (IO ()))

foreign import ccall safe "Function_call_f1"
   c_Function_call_f1 :: Ptr (RawFunction (IO ())) -> IO ()
