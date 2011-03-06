{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, FlexibleInstances, 
             FlexibleContexts, UndecidableInstances, GADTs, ScopedTypeVariables #-}

import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr 
import Foreign.ForeignPtr 


class Castable a b where
  cast :: a -> b 
  uncast :: b -> a 

instance Castable Int CInt where
  cast = fromIntegral 
  uncast = fromIntegral
  
instance Castable Double CDouble where
  cast = realToFrac
  uncast = realToFrac 
  

instance (Castable a a', Castable b b') => Castable (a->b) (a'->b') where
  cast f = cast . f . uncast
  uncast f = uncast . f . cast 

{-
class IOCastable a b where
  iocast :: a -> IO b 
  uniocast :: b -> IO a

instance (Castable a b) => IOCastable a b where
  iocast = return . cast 
  uniocast = return . uncast
  
instance (IOCastable a a', IOCastable b b') => IOCastable (a->b) (a'->b') where
  iocast f = \x' -> do x <- uniocast x' 
                       iocast (f x) 
  uniocast f' = \x -> do x' <- iocast x                  
                         uniocast (f' x')

instance (IOCastable a a', IOCastable b b') => IOCastable (a->IO b) (a'->IO b') where
  iocast f = \x' -> do x <- uniocast x'
                       y <- f x 
                       iocast y 
  uniocast f' = \x -> do x' <- iocast x
                         y' <- f' x' 
                         uniocast y' -}

instance Castable String CString where
  cast x = unsafePerformIO (newCString x)
  uncast x = unsafePerformIO (peekCString x) 



f :: Int -> Int 
f = (+1)

g :: Int -> Int -> Int 
g = (+)

h :: Int -> Int -> Double-> Double 
h x y z = fromIntegral x + fromIntegral y + z

cone :: CInt 
cone = 1 

cdouble :: CDouble 
cdouble = 2.0 

main = do 
  putStrLn "test" 
  print $ ((cast f) cone :: CInt)                   
  print $ ((cast g) cone cone :: CInt ) 
  print $ ((cast h) cone cone cdouble :: CDouble)
{-
type family T a :: *
type instance T Int = CInt
type instance T Double = CDouble  
type instance T (a->b) = T a -> T b 
-}


{-


data Wrap a b  where
  WrapInt    :: CInt -> Wrap Int CInt 
  WrapDouble :: CDouble -> Wrap Double CDouble 
  WrapFunction :: (Castable a a', Castable b b') => (a -> b) -> Wrap (a -> b)  (a' -> b') 


wrap_ :: T a -> Wrap a                   
wrap_  ( x :: T Int ) = WrapInt x
                  
unwrap_ :: Wrap a -> T a
unwrap_ (WrapInt x) = x                
unwrap_ (WrapDouble x) = x 
unwrap_ (WrapFunction f) = f
                  

class (WFunctor m) => WApplicative m where 
  wpure :: (Wrappable a) => a -> m a 
  (<=*=>)  :: (Wrappable a, Wrappable b) => m (a->b) -> m a -> m b


instance Wrappable Int where
  wrap = WrapInt . fromIntegral
  unwrap = fromIntegral . unwrap_
 
instance Wrappable Double where
  wrap = WrapDouble . realToFrac
  unwrap = realToFrac . unwrap_

instance (Wrappable a, Wrappable b) => Wrappable (a->b) where
  wrap f = WrapFunction ( wrap . f . unwrap_ . wrap )   
  unwrap (WrapFunction f) = unwrap_ . f . wrap   

-}

{-

instance WFunctor Wrap where
  wfmap f = wrap . f . unwrap 
  





instance WApplicative Wrap where
  wpure = wrap 
  f_  <=*=> x_ = wrap ((unwrap f_) (unwrap x_))

  
data Lifted a where
  LiftedInt    :: CInt -> Lifted Int   
  LiftedDouble :: CDouble -> Lifted Double
  LiftedFunction :: (T a -> T b ) -> Lifted ( a -> b) 

-}

{-


instance WApplicative NewType where
  wpure = castupM 


class Injectable a where
  inject :: T a -> NewType a 
  
instance Injectable Int where 
  inject = NewInt 
  
instance Injectable Double where
  inject = NewDouble 



class MCastable m where  
  castupM :: (Wrappable a) => a -> m a 
  castdownM :: (Wrappable a) => m a -> a
--  escapeM :: m a -> T a
--  injectM :: (Injectable a) => T a -> m a 
  
  
          
instance (MCastable m) => WFunctor m where  
  wfmap f = castupM . f . castdownM 
  
instance (MCastable m) => WApplicative m where 
  wpure = castupM 
  (m <=*=>  = 


castupW :: Wrap a -> NewType a   
castupW (WrapInt x) = NewInt (fromIntegral x)
castupW (WrapDouble x) = NewDouble (realToFrac x)
castupW (WrapFunction f) = let g x = f (castupW x)  wrap 
                                     
                                     
                                     NewFunction f w 


castdownW :: NewType a -> Wrap a 
castdownW (NewInt x) = WrapInt (fromIntegral x)
castdownW (NewDouble x) = WrapDouble (realToFrac x)
  
instance MCastable NewType where
  castupM x = castupW (wrap x) 
  castdownM x = unwrap (castdownW x)            
  escapeM = unnew 
  injectM = inject 
              
-}

