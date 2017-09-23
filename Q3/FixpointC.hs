module FixpointC where

newtype Mu a = Roll (Mu a -> (a -> a))
unroll (Roll x) = x

fix :: (a -> a) -> a -> a
fix = \f ->       (\x z -> f ((unroll x) x z))
            (Roll (\x z -> f ((unroll x) x z)))

facF :: (Int -> Int) -> Int -> Int
facF f x
  | x <= 0    = 1
  | otherwise = x * (f (x-1))

fac :: Int -> Int
fac = fix facF undefined

teste = fix (\x -> x + 1) 4
