module Lib
    ( run1
    , run2
    ) where

-- express any monoid as a list in some "language"
runMonoid :: Monoid b => (a -> b) -> [a] -> b
runMonoid f [] = mempty
runMonoid f (a:as) = mappend (f a) (runMonoid f as)

data Move = Back Integer | Forth Integer

moves = [Forth 7, Forth 3, Back 1, Forth 2, Back 19]

instance Monoid Integer where
  mempty = 0
  mappend = (+)

sem1 :: Move -> Integer
sem1 (Forth x) = x
sem1 (Back x) = -x

run1 = runMonoid sem1 moves

sem2 :: Move -> [String]
sem2 (Forth x) = ["forward " ++ show x]
sem2 (Back x) = ["backward " ++ show x]

run2 = runMonoid sem2 moves
