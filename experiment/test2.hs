{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  A :: a -> BHList l m n a -> BHList (S l) m (S n) a


x :: BHList (T 3) (T 2) (T 5) Int
x = 1 `A` (2 `A` (3 `A` (M (M E))))



y :: BHList (T 0) (T 1) (T 1) Int
y = M E 


f :: forall m. forall n. BHList (S (S (S m))) n  (T 5) Int -> Int
f (x `A` (y `A` (z `A` (M (M E))))) = x + y + z
f (x `A` (y `A` (z `A` (o1 `A` (M E))))) = x + y + z + o1
f (x `A` (y `A` (z `A` (o1 `A` (o2 `A` E))))) = x + y + z + o1 + o2


main :: IO ()
main = putStrLn "test"
