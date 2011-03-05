{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, FlexibleInstances, 
             FlexibleContexts, UndecidableInstances, GADTs #-}

import Foreign.C
import Foreign.Ptr 
import Foreign.ForeignPtr 

{-
type family T a :: *
     
data NewType a = NewType (T a) (a -> T a) (T a -> a) 

fmap' :: (a -> a) -> NewType a -> NewType a
fmap' f (NewType x castup castdown) = NewType (castup (f (castdown x))) castup castdown 
-}


-- type family T a :: * 
     
{-
castupT   :: a -> T a
castdownT :: T a -> a 
-}

{-
class Castable a b where
  castup :: a -> b 
  castdown :: b -> a -}
    
class Wrappable a where
  wrap :: a -> Wrap a 

instance Wrappable Int where
  wrap = WrapInt
 
instance Wrappable Double where
  wrap = WrapDouble 


class Injectable a where
  inject :: T a -> NewType a 
  
instance Injectable Int where 
  inject = NewInt 
  
instance Injectable Double where
  inject = NewDouble 


type family T a :: *
type instance T Int = CInt
type instance T Double = CDouble  


class MCastable m where  
  castupM :: (Wrappable a) => a -> m a 
  castdownM :: (Wrappable a) => m a -> a
  escapeM :: m a -> T a
  injectM :: (Injectable a) => T a -> m a 
  
class WFunctor m where   
  wfmap :: (Wrappable a, Wrappable b) => (a -> b) -> m a -> m b 
  
          
instance (MCastable m) => WFunctor m where  
  wfmap f = castupM . f . castdownM 
  
data Wrap a where
  WrapInt    :: Int -> Wrap Int
  WrapDouble :: Double -> Wrap Double

unwrap :: Wrap a -> a 
unwrap (WrapInt x) = x
unwrap (WrapDouble x) = x 

data NewType a where
  NewInt    :: CInt -> NewType Int   
  NewDouble :: CDouble -> NewType Double
  
unnew :: NewType a -> T a 
unnew (NewInt x) = x 
unnew (NewDouble x) = x 

castupW :: Wrap a -> NewType a   
castupW (WrapInt x) = NewInt (fromIntegral x)
castupW (WrapDouble x) = NewDouble (realToFrac x)

castdownW :: NewType a -> Wrap a 
castdownW (NewInt x) = WrapInt (fromIntegral x)
castdownW (NewDouble x) = WrapDouble (realToFrac x)
  
instance MCastable NewType where
  castupM x = castupW (wrap x) 
  castdownM x = unwrap (castdownW x)            
  escapeM = unnew 
  injectM = inject 
              
main = do 
  putStrLn "test" 
   