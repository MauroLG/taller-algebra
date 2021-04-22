module Clase04
where
import Clase03

{- Ejercicios: otras sumatorias -}

sumatoria :: Int -> Int 
sumatoria n | n == 0 = 0
            | n >  0 = n + sumatoria (n-1)

f1 :: Int -> Int
f1 n | n == 0 = 1
     | n >  0 = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float 
f2 n q | n == 1 = q
       | n > 0 = q^n + f2(n-1) q

f3 :: Int -> Float -> Float 
f3 n q | n == 0 = 1
       | n >  0 = q^(2*n-1) + q^(2*n) + f3 (n-1) q

f4 :: Int -> Float -> Float
f4 n q | n == 0 = 1
       | n >  0 = q^(2*n-1) + q^(2*n) - q^(n-1) + f3 (n-1) q

eAprox :: Int -> Float 
eAprox n | n == 0 = 1
         | n == n = eAprox (n-1) + 1 / fromIntegral (factorial n)

e :: Float 
e = eAprox 10

f :: Int -> Int -> Int
f n m | n == 0 = 0
      | n >  0 = f (n-1) m + round (f2 m (fromIntegral n))

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n m | m == 0 = 0
                    | m >  0 = (sumaPotencias q n (m-1)) + q^m*(f2 n q)

sumaRacionales :: Int -> Int -> Float 
sumaRacionales n m | m == 0 = 0 
                   | m >  0 = sumaRacionales n (m-1) + fromIntegral(sumatoria n) / fromIntegral m 

g1 :: Int -> Int -> Int
g1 i n | n < i = 0
       | otherwise = g1 i (n-1) + i ^ n

faux :: Int -> Int -> Int
faux n m | m == 1 = 1
         | otherwise = faux n (m-1) + m ^ n

g2 :: Int -> Int
g2 n | n == 1 = 1
     | otherwise = g2 (n-1) + faux n n 

g3 :: Int -> Int
g3 n | n <= 0 = 0
     | n `mod` 2 == 0 = 2^n + g3 (n-2)
     | n `mod` 2 /= 0 = g3 (n-1)

sumaIguales :: Int -> Int
sumaIguales n | n <= 0 = 0
              | digitosIguales n == False = sumaIguales(n-1)
              | digitosIguales n == True = sumaIguales(n-1) + n