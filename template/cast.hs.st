{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, FlexibleInstances, 
             FlexibleContexts, UndecidableInstances, GADTs, ScopedTypeVariables #-}

import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr 
import Foreign.ForeignPtr 


class Castable a b where
  cast :: a -> b 
  uncast :: b -> a 

instance Castable a a where
  cast = id
  uncast = id

instance Castable Int CInt where
  cast = fromIntegral 
  uncast = fromIntegral

instance Castable Word CUInt where
  cast = fromIntegral
  uncast = fromIntegral
  
instance Castable Double CDouble where
  cast = realToFrac
  uncast = realToFrac 

instance Castable String CString where
  cast x = unsafePerformIO (newCString x)
  uncast x = unsafePerformIO (peekCString x) 

  
instance (FPtr a) => Castable a (Ptr (Raw a)) where
  cast = unsafeForeignPtrToPTr.get_fptr
  uncast = cast_fptr_to_obj . newForeignPtr_ 

instance (Castable a a', Castable b b') => Castable (a->b) (a'->b') where
  cast f = cast . f . uncast
  uncast f = uncast . f . cast 


xform0 :: (FPtr a, Castable y cy) 
       => (Ptr (Raw a) -> IO cy) -> a -> IO y
xform0 f a = f (cast a) >>= return . uncast 

xform1 :: (FPtr a, Castable x1 cx1, Castable y cy) 
       => (Ptr (Raw a) -> cx1 -> IO cy) -> a -> x1 -> IO y
xform1 f a x1 = f (cast a) (cast x1) >>= return . uncast 

xform2 :: (FPtr a, Castable x1 cx1, Castable x2 cx2, Castable y cy) 
       => (Ptr (Raw a) -> cx1 -> cx2 -> IO cy) -> a -> x1 -> x2-> IO y
xform2 f a x1 x2 = f (cast a) (cast x1) (cast x2) >>= return . uncast 

xform3 :: (FPtr a, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable y, cy) 
       => (Ptr (Raw a) -> cx1 -> cx2 -> cx3 -> IO cy1) -> a -> x1 -> x2 -> x3 -> IO y
xform3 f a x1 x2 x3 = (f (cast a) (cast x1) (cast x2) (cast x3) >>= return . uncast 

xform4 :: (FPtr a, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4, Castable y cy) 
       => (Ptr (Raw a) -> cx1 -> cx2 -> cx3 -> cx4 -> IO cy) -> a -> x1 -> x2 -> x3 -> x4 -> IO y
xform4 f a x1 x2 x3 x4 = (f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) >>= return . uncast 