{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits 

type family T (a :: Nat) :: N

type instance T 0 = Z 
type instance T 1 = S Z
type instance T 2 = S (S Z)
type instance T 3 = S (S (S Z))
type instance T 4 = S (S (S (S Z)))
type instance T 5 = S (S (S (S (S Z))))

data N where
  Z :: N
  S :: N -> N

data BHList :: N -> N -> N -> * -> * where
  E :: BHList Z Z Z a
  M :: BHList l m n a -> BHList l (S m) (S n) a
  (:+) :: a -> BHList l m n a -> BHList (S l) m (S n) a

infixr 9 :+

{-
class Emptyable m where
  empty :: m 


instance Emptyable (BHList Z Z Z a) where
  empty = E


instance Emptyable (BHList Z (S Z) (S Z) a) where
  empty = M E
-}

{-
instance (Emptyable (BHList Z n n a)) => 
           Emptyable (BHList Z (S n) (S n) a) where
  empty = M empty
-}

-- g :: BHList Z n n a 
-- g = 

x :: BHList (T 3) (T 2) (T 5) Int
x = 1 :+ 2 :+ 3 :+ (M (M E))



y :: BHList (T 0) (T 1) (T 1) Int
y = M E 


f :: forall m. forall n. BHList (S (S (S m))) n  (T 5) Int -> Int
f (x :+ y :+ z :+ M (M E)) = x + y + z
f (x :+ y :+ z :+ o1 :+ (M E)) = x + y + z + o1
f (x :+ y :+ z :+ o1 :+ o2 :+ E) = x + y + z + o1 + o2


main :: IO ()
main = do
  print (f (1 :+ 2 :+ 3 :+ M (M E)))
  print (f (1 :+ 2 :+ 3 :+ 4 :+ M E))
  print (f (1 :+ 2 :+ 3 :+ 4 :+ 5 :+ E))
