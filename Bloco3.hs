module Bloco3 where

--1) Funcao que recebe um inteiro e retorna uma tupla 
multiplos :: Int -> (Int, Int, Int, Int)
multiplos n = (n * 2, n * 3, n * 4, n * 5)

--2) Recebe um numero e retorna uma tupla com divisao inteira por 2, par/impar
parImpar :: Int -> (Int, String)
parImpar n = (n `div` 2, paridade)
    where 
        paridade = if n `mod` 2 == 0 then "Par" else "Impar"

--3) Funcao que recebe duas tuplas (x1, y1) (x2, y2) e retorna a soma vetorial
somaVetorial :: (Float, Float) -> (Float, Float) -> (Float, Float)
somaVetorial (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--4) Funcao que recebe (base, altura) e retorna area e perimetro do retangulo
areaAltura :: (Int, Int) -> (Int, Int)
areaAltura (base, altura) = (area, perimetro)
    where
        area = base * altura
        perimetro = 2 * base + 2 * altura

--5) Recebe tupla com 3 numeros e retorna maior, menor e media
analisaTupla :: (Float, Float, Float) -> (Float, Float, Float)
analisaTupla (a, b, c) = (maior, menor, media)
    where
        media = (a + b + c) / 3

        maior
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | otherwise        = c
        
        menor
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | otherwise        = c
