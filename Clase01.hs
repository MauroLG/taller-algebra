module Clase01
where

-- Ejercicio 1 - absoluto

absoluto x = abs x

absolutoAlt x | x >= 0 = x
              | otherwise = x*(-1)

-- Ejercicio 2 - maximoabsoluto

maximoAbsoluto x y | absolutoAlt x >= absolutoAlt y = x
                   | otherwise = y

-- Ejercicio 3 - maximo3

maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z

-- Ejercicio 4 - algunoEs0

algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False

algunoEs0Alt 0 y = True
algunoEs0Alt x 0 = True
algunoEs0Alt _ _ = False

-- Ejercicio 5 - ambosSon0

ambosSon0 x y | x && y == 0 = True 
              | otherwise = False 

ambosSon0Alt 0 0 = True 
ambosSon0Alt _ _ = False

-- Ejercicio 6 - esMultiploDe

esMultiploDe x y = mod x y == 0

-- Ejercicio 7 - digitoUnidades

digitoUnidades x = mod x 10

-- Ejercicio 8 - digitoDecenas
digitoDecenas x = mod(div x 10) 10