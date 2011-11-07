{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, MultiParamTypeClasses, 
             FlexibleInstances, TypeSynonymInstances, 
             EmptyDataDecls, ExistentialQuantification, ScopedTypeVariables, 
             GADTs #-}

module HROOT.TypeCast where

import Data.Word
import Foreign.C            
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import System.IO.Unsafe

class Castable a b where
  cast :: a -> b 
  uncast :: b -> a 

class FPtr a where
  type Raw a :: *
  get_fptr :: a -> ForeignPtr (Raw a) 
  cast_fptr_to_obj :: ForeignPtr (Raw a) -> a

class Existable a where
  data Exist a :: *  

data BottomType

class GADTTypeable a where
  data GADTType a :: * -> *
  data EGADTType a :: *




{-
-- eliminate this for the time being to have a solution with Repl
instance Castable a a where
  cast = id
  uncast = id
-}

instance Castable () () where
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

instance Castable [Double] (Ptr CDouble) where
  cast xs = unsafePerformIO (newArray (map realToFrac xs))
  uncast _c_xs = undefined 

instance Castable [Int] (Ptr CInt) where
  cast xs = unsafePerformIO (newArray (map fromIntegral xs))
  uncast _c_xs = undefined 

instance Castable String CString where
  cast x = unsafePerformIO (newCString x)
  uncast x = unsafePerformIO (peekCString x) 

instance Castable [String] (Ptr CString) where
  cast xs = unsafePerformIO (mapM  newCString xs >>= newArray)
  uncast _c_xs = undefined

instance (Castable a a', Castable b b') => Castable (a->b) (a'->b') where
  cast f = cast . f . uncast
  uncast f = uncast . f . cast 


xformnull :: (Castable a ca) => (IO ca) -> IO a
xformnull f = f >>= return . uncast

xform0 :: (Castable a ca, Castable y cy) 
       => (ca -> IO cy) -> a -> IO y
xform0 f a = f (cast a) >>= return . uncast 

xform1 :: (Castable a ca, Castable x1 cx1, Castable y cy) 
       => (ca -> cx1 -> IO cy) -> a -> x1 -> IO y
xform1 f a x1 = f (cast a) (cast x1) >>= return . uncast 

xform2 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable y cy) 
       => (ca -> cx1 -> cx2 -> IO cy) -> a -> x1 -> x2-> IO y
xform2 f a x1 x2 = f (cast a) (cast x1) (cast x2) >>= return . uncast 

xform3 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> IO cy) -> a -> x1 -> x2 -> x3 -> IO y
xform3 f a x1 x2 x3 = f (cast a) (cast x1) (cast x2) (cast x3) >>= return . uncast 

xform4 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> IO cy) -> a -> x1 -> x2 -> x3 -> x4 -> IO y
xform4 f a x1 x2 x3 x4 = f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) >>= return . uncast 

xform5 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> IO cy) -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> IO y
xform5 f a x1 x2 x3 x4 x5 = f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) >>= return . uncast 

xform6 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> IO y
xform6 f a x1 x2 x3 x4 x5 x6 = 
  f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) (cast x6) 
  >>= return . uncast 

xform7 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> IO y
xform7 f a x1 x2 x3 x4 x5 x6 x7 = 
  f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) (cast x6) (cast x7)
  >>= return . uncast 

xform8 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> IO y
xform8 f a x1 x2 x3 x4 x5 x6 x7 x8 = 
  f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) (cast x6) (cast x7) (cast x8)
  >>= return . uncast 

xform9 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable x9 cx9, 
           Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> cx9 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> IO y
xform9 f a x1 x2 x3 x4 x5 x6 x7 x8 x9 = 
  f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) (cast x6) (cast x7) (cast x8) (cast x9)
  >>= return . uncast 

xform10 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
            Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable x9 cx9,  
            Castable x10 cx10, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> cx9 -> cx10 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> IO y
xform10 f a x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = 
  f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) (cast x6) (cast x7) (cast x8) (cast x9) (cast x10) >>= return . uncast 

xform11 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
            Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable x9 cx9,  
            Castable x10 cx10, Castable x11 cx11, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> cx9 -> cx10 -> cx11 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> IO y
xform11 f a x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = 
  f (cast a) (cast x1) (cast x2) (cast x3) (cast x4) (cast x5) (cast x6) (cast x7) (cast x8) (cast x9) (cast x10) (cast x11)  >>= return . uncast 

