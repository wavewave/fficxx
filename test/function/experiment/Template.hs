{-# LANGUAGE EmptyDataDecls, FlexibleInstances,
  MultiParamTypeClasses, TypeFamilies #-}
module Template where
import Foreign.C.Types
import Foreign.Ptr
import FFICXX.Runtime.Cast

data RawFunction t

newtype Function t = Function (Ptr (RawFunction t))

class () => IFunction t where
  newFunction :: FunPtr t -> IO (Function t)
  call :: Function t -> t

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


