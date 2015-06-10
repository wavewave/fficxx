{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits 

type family EQ (a :: Nat) :: N

type instance EQ 0 = Z 
type instance EQ 1 = S Z
type instance EQ 2 = S (S Z)
type instance EQ 3 = S (S (S Z))
type instance EQ 4 = S (S (S (S Z)))
type instance EQ 5 = S (S (S (S (S Z))))

type family GTEQ (n :: N) (m :: Nat) :: N

type instance GTEQ n 0 = n
type instance GTEQ n 1 = S n
type instance GTEQ n 2 = S (S n)
type instance GTEQ n 3 = S (S (S n))
type instance GTEQ n 4 = S (S (S (S n)))


data N where
  Z :: N
  S :: N -> N

data BHList :: N -> N -> N -> * -> * where
  E :: BHList Z Z Z a
  M :: BHList l m n a -> BHList l (S m) (S n) a
  (:+) :: a -> BHList l m n a -> BHList (S l) m (S n) a

infixr 9 :+

x :: BHList (EQ 3) (EQ 2) (EQ 5) Int
x = 1 :+ 2 :+ 3 :+ (M (M E))

y :: BHList (EQ 0) (EQ 1) (EQ 1) Int
y = M E 


f :: forall m. forall n. BHList (GTEQ m 3) n (EQ 5) Int -> Int
f (x :+ y :+ z :+ M (M E)) = x + y + z
f (x :+ y :+ z :+ o1 :+ (M E)) = x + y + z + o1
f (x :+ y :+ z :+ o1 :+ o2 :+ E) = x + y + z + o1 + o2


main :: IO ()
main = do
  print (f (1 :+ 2 :+ 3 :+ M (M E)))
  print (f (1 :+ 2 :+ 3 :+ 4 :+ M E))
  print (f (1 :+ 2 :+ 3 :+ 4 :+ 5 :+ E))
