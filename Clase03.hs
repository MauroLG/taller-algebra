module Clase03
where

factorial :: Int -> Int 
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

fib :: Int -> Int 
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)

parteEntera :: Float -> Int
parteEntera x | x < 1 = 0
              | otherwise = parteEntera (x-1) + 1  

multiploDeTres :: Int -> Bool 
multiploDeTres n | n == 3 = True 
                 | n <  3 = False 
                 | n >  3 = multiploDeTres(n-3)

sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1 
              | n >  1 = sumaImpares(n-1)+n*2-1

medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n >  1 = n * medioFact(n-2)

sumaDigitos :: Int -> Int
sumaDigitos n | n <= 0 = undefined
              | n < 10 = n
              | otherwise = (n `mod` 10) + sumaDigitos(n `div` 10)

digitosIguales :: Int -> Bool 
digitosIguales n | n < 0 = undefined 
                 | n < 10 = True
                 | mod n 10 == mod (div n 10) 10 = digitosIguales (div n 10)
                 | otherwise = False 