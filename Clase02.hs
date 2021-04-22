module Clase02
where

{- Ejercicio 1: Se pide decidir si dos numeros en R están relacionados considerando las clases de equivalencia dadas.
Al ser R, los argumentos son del tipo Float y el return debe ser un Bool -}
estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | (x<=3) && (y<=3) = True
                      | (x>3) && (x<=7) && (y>3) && (y<=7) = True
                      | (x>7) && (y>7) = True
                      | otherwise = False

{- Ejercicio 2: Se pide calcular el producto escalar o interno de dos vectores en R2. Se utilizan pares para representar los vectores, así como también los tipos -}
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1,y1) (x2, y2) = x1 * x2 + y1 * y2

{- Ejercicio 3: Se pide decidir si cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector. Se utilizan pares para representar a los tipos y funciones de acceso para acceder a cada coordenada de los vectores en R2. Finalmente se devuelve un Bool -}
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor v1 v2 | fst v1 < fst v2 && snd v1 < snd v2 = True
                | otherwise = False 

{- Ejercicio 4: Se pide calcular la distancia entre dos puntos en R2. Se utilizan pares para representar a los tipos y funciones de acceso para cada coordenada de los puntos dados -}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float 
distanciaPuntos p1 p2 = sqrt((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)

{- Ejercicio 5: Se pide sumar los elementos de una terna. Se utiliza una terna para representar a los tipos de los argumentos -}
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a + b + c

{- Ejercicio 6: Se pide devolver la posición del primer numero par que se encuentre en una terna de numeros enteros o devolver el numero 4 si ninguno es par. Se utiliza una terna para representar a los tipos de los argumentos -}
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (a, b, c) | a `mod` 2 == 0 = 1
                         | b `mod` 2 == 0 = 2
                         | c `mod` 2 == 0 = 3
                         | otherwise = 4

{- Ejercicio 7: Se pide crear un par a partir de dos argumentos dados por separado -}
crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

{- Ejercicio 8: Se pide invertir los elementos del par pasado como argumento y retornarlo -}
invertir :: (a,b) -> (b, a)
invertir (a,b) = (b,a)