{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}


-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Runtime.Cast
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Runtime.Cast where

import Data.String
import Data.Word
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import System.IO.Unsafe

class IsRawType a 

class Castable a b where
  cast :: a -> b 
  uncast :: b -> a 

class FPtr a where
  type Raw a :: *
  get_fptr :: a -> ForeignPtr (Raw a) 
  cast_fptr_to_obj :: ForeignPtr (Raw a) -> a


class FunPtrWrappable a where
  type FunPtrHsType a :: *
  type FunPtrType a :: *
  data FunPtrWrapped a :: *  
  fptrWrap :: FunPtrWrapped a -> IO (FunPtr (FunPtrType a)) 
  wrap :: FunPtrHsType a -> FunPtrWrapped a 

class Existable a where
  data Exist a :: *  

data BottomType

class GADTTypeable a where
  data GADTType a :: * -> *
  data EGADTType a :: *

class IsCType a where 


instance IsCType CChar
instance IsCType CInt
instance IsCType CUInt 
instance IsCType CString 
instance IsCType CULong 
instance IsCType CLong

instance IsString CString where
  fromString str = unsafePerformIO (newCString str)

-- cause incoherent instances but cannot avoid it now
{-
instance Castable a a where
  cast = id
  uncast = id
-}

instance Castable () () where
  cast = id 
  uncast = id 


instance Castable CDouble CDouble where
  cast = id
  uncast = id



instance Castable CUInt CUInt where
  cast = id 
  uncast = id 

instance Castable CInt CInt where 
  cast = id 
  uncast = id 

instance Castable CLong CLong where
  cast = id
  uncast = id

instance Castable CULong CULong where 
  cast = id 
  uncast = id 

 
instance Castable (Ptr CInt) (Ptr CInt) where 
  cast = id 
  uncast = id 

instance Castable (Ptr CChar) (Ptr CChar) where 
  cast = id 
  uncast = id 

instance Castable (Ptr CUInt) (Ptr CUInt) where 
  cast = id 
  uncast = id 

instance Castable (Ptr CULong) (Ptr CULong) where 
  cast = id 
  uncast = id 

instance Castable (Ptr CLong) (Ptr CLong) where 
  cast = id 
  uncast = id 

instance Castable (Ptr CDouble) (Ptr CDouble) where 
  cast = id 
  uncast = id 

instance Castable (Ptr CString) (Ptr CString) where 
  cast = id 
  uncast = id 

instance Castable (Ptr ()) (Ptr ()) where
  cast = id
  uncast = id


instance Castable Int CInt where
  cast = fromIntegral 
  uncast = fromIntegral

instance Castable Word CUInt where
  cast = fromIntegral
  uncast = fromIntegral

instance Castable Word8 CChar where
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

