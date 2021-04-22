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