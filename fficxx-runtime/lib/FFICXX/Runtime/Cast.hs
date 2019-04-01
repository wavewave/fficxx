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
-- Copyright   : (c) 2011-2017 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Runtime.Cast where

import Control.Monad         ((>=>))
import Data.ByteString.Char8 (ByteString,packCString, useAsCString)
import Data.String
import Data.Word
import Foreign.C
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
-- import System.IO.Unsafe


class IsRawType a 

class Castable a b where
  cast   :: a -> (b -> IO r) -> IO r 
  uncast :: b -> (a -> IO r) -> IO r 

class FPtr a where
  type Raw a :: *
  get_fptr :: a -> Ptr (Raw a) 
  cast_fptr_to_obj :: Ptr (Raw a) -> a


class FunPtrWrappable a where
  type FunPtrHsType a :: *
  type FunPtrType a :: *
  data FunPtrWrapped a :: *  
  fptrWrap :: FunPtrWrapped a -> IO (FunPtr (FunPtrType a)) 
  wrap :: FunPtrHsType a -> FunPtrWrapped a 


class IsCType a where 

instance IsCType CChar
instance IsCType CInt
instance IsCType CUInt 
instance IsCType CString 
instance IsCType CULong 
instance IsCType CLong

instance Castable () () where
  cast x f = f x
  uncast x f = f x

instance Castable CChar CChar where
  cast x f = f x
  uncast x f = f x

instance Castable CDouble CDouble where
  cast x f = f x
  uncast x f = f x

instance Castable CUInt CUInt where
  cast x f = f x 
  uncast x f = f x  

instance Castable CInt CInt where 
  cast x f = f x 
  uncast x f = f x 

instance Castable CLong CLong where
  cast x f = f x 
  uncast x f = f x

instance Castable CULong CULong where 
  cast x f = f x 
  uncast x f = f x 

 
instance Castable (Ptr CInt) (Ptr CInt) where 
  cast x f = f x
  uncast x f = f x 

instance Castable (Ptr CChar) (Ptr CChar) where 
  cast x f = f x 
  uncast x f = f x 

instance Castable (Ptr CUInt) (Ptr CUInt) where 
  cast x f = f x 
  uncast x f = f x 

instance Castable (Ptr CULong) (Ptr CULong) where 
  cast x f = f x 
  uncast x f = f x 

instance Castable (Ptr CLong) (Ptr CLong) where 
  cast x f = f x 
  uncast x f = f x 

instance Castable (Ptr CDouble) (Ptr CDouble) where 
  cast x f = f x 
  uncast x f = f x 

instance Castable (Ptr CString) (Ptr CString) where 
  cast x f = f x 
  uncast x f = f x 

instance Castable (Ptr ()) (Ptr ()) where
  cast x f = f x
  uncast x f = f x


instance Castable Int CInt where
  cast x f = f (fromIntegral x)
  uncast x f = f (fromIntegral x)

instance Castable Word CUInt where
  cast x f = f (fromIntegral x)
  uncast x f = f (fromIntegral x)

instance Castable Word8 CChar where
  cast x f = f (fromIntegral x)
  uncast x f = f (fromIntegral x)
  
instance Castable Double CDouble where
  cast x f = f (realToFrac x)
  uncast x f = f (realToFrac x)

instance Castable [Double] (Ptr CDouble) where
  cast xs f = newArray (map realToFrac xs) >>= f 
  uncast xs f = undefined 

instance Castable [Int] (Ptr CInt) where
  cast xs f = newArray (map fromIntegral xs) >>= f
  uncast xs f = undefined 

instance Castable ByteString CString where
  cast x f = useAsCString x f
  uncast x f = packCString x >>= f 


instance Castable [ByteString] (Ptr CString) where
  cast xs f = do ys <- mapM (\x -> useAsCString x return) xs
                 withArray ys $ \cptr -> f cptr
  uncast xs f = undefined 

{- 
instance Castable String CString where
  cast x = unsafePerformIO (newCString x)
  uncast x = unsafePerformIO (peekCString x) 


instance (Castable a a', Castable b b') => Castable (a->b) (a'->b') where
  cast f = cast . f . uncast
  uncast f = uncast . f . cast 
-}

xformnull :: (Castable a ca) => (IO ca) -> IO a
xformnull f = f >>= \ca -> uncast ca return

xform0 :: (Castable a ca, Castable y cy) 
       => (ca -> IO cy) -> a -> IO y
xform0 f a = cast a $ \ca -> f ca >>= \cy -> uncast cy return

xform1 :: (Castable a ca, Castable x1 cx1, Castable y cy) 
       => (ca -> cx1 -> IO cy) -> a -> x1 -> IO y
xform1 f a x1 = cast a $ \ca ->
                  cast x1 $ \cx1 ->
                    f ca cx1 >>= \cy -> uncast cy return 

xform2 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable y cy) 
       => (ca -> cx1 -> cx2 -> IO cy) -> a -> x1 -> x2-> IO y
xform2 f a x1 x2 = cast a $ \ca ->
                     cast x1 $ \cx1 ->
                       cast x2 $ \cx2 ->
                         f ca cx1 cx2 >>= \cy -> uncast cy return

xform3 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> IO cy) -> a -> x1 -> x2 -> x3 -> IO y
xform3 f a x1 x2 x3 = cast a $ \ca ->
                        cast x1 $ \cx1 ->
                          cast x2 $ \cx2 ->
                            cast x3 $ \cx3 ->
                              f ca cx1 cx2 cx3 >>= \cy -> uncast cy return 

xform4 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> IO cy) -> a -> x1 -> x2 -> x3 -> x4 -> IO y
xform4 f a x1 x2 x3 x4 =
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 -> 
            f ca cx1 cx2 cx3 cx4 >>= \cy -> uncast cy return 


xform5 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> IO cy) -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> IO y
xform5 f a x1 x2 x3 x4 x5 =
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 -> 
              f ca cx1 cx2 cx3 cx4 cx5 >>= \cy -> uncast cy return 

xform6 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> IO y
xform6 f a x1 x2 x3 x4 x5 x6 = 
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 ->
              cast x6 $ \cx6 -> 
                f ca cx1 cx2 cx3 cx4 cx5 cx6 >>= \cy -> uncast cy return 
  
xform7 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> IO y
xform7 f a x1 x2 x3 x4 x5 x6 x7 = 
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 ->
              cast x6 $ \cx6 ->
                cast x7 $ \cx7 ->  
                  f ca cx1 cx2 cx3 cx4 cx5 cx6 cx7 >>= \cy -> uncast cy return 


xform8 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> IO y
xform8 f a x1 x2 x3 x4 x5 x6 x7 x8 =
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 ->
              cast x6 $ \cx6 ->
                cast x7 $ \cx7 ->
                  cast x8 $ \cx8 ->  
                    f ca cx1 cx2 cx3 cx4 cx5 cx6 cx7 cx8 >>= \cy -> uncast cy return 

xform9 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
           Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable x9 cx9, 
           Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> cx9 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> IO y
xform9 f a x1 x2 x3 x4 x5 x6 x7 x8 x9 = 
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 ->
              cast x6 $ \cx6 ->
                cast x7 $ \cx7 ->
                  cast x8 $ \cx8 ->
                    cast x9 $ \cx9 ->  
                      f ca cx1 cx2 cx3 cx4 cx5 cx6 cx7 cx8 cx9 >>= \cy -> uncast cy return 

xform10 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
            Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable x9 cx9,  
            Castable x10 cx10, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> cx9 -> cx10 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> IO y
xform10 f a x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = 
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 ->
              cast x6 $ \cx6 ->
                cast x7 $ \cx7 ->
                  cast x8 $ \cx8 ->
                    cast x9 $ \cx9 ->
                      cast x10 $ \cx10 ->  
                        f ca cx1 cx2 cx3 cx4 cx5 cx6 cx7 cx8 cx9 cx10 >>= \cy -> uncast cy return 

xform11 :: (Castable a ca, Castable x1 cx1, Castable x2 cx2, Castable x3 cx3, Castable x4 cx4,
            Castable x5 cx5, Castable x6 cx6, Castable x7 cx7, Castable x8 cx8, Castable x9 cx9,  
            Castable x10 cx10, Castable x11 cx11, Castable y cy) 
       => (ca -> cx1 -> cx2 -> cx3 -> cx4 -> cx5 -> cx6 -> cx7 -> cx8 -> cx9 -> cx10 -> cx11 -> IO cy) 
          -> a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> IO y
xform11 f a x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
  cast a $ \ca ->
    cast x1 $ \cx1 ->
      cast x2 $ \cx2 ->
        cast x3 $ \cx3 ->
          cast x4 $ \cx4 ->
            cast x5 $ \cx5 ->
              cast x6 $ \cx6 ->
                cast x7 $ \cx7 ->
                  cast x8 $ \cx8 ->
                    cast x9 $ \cx9 ->
                      cast x10 $ \cx10 ->
                        cast x11 $ \cx11 ->  
                         f ca cx1 cx2 cx3 cx4 cx5 cx6 cx7 cx8 cx9 cx10 cx11 >>= \cy -> uncast cy return 

