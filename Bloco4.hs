module Bloco4 where

--1) Soma dos numeros positivos e produto dos negativos
somaEProduto :: [Int] -> (Int, Int)
somaEProduto [] = (0, 0)
somaEProduto (h:tail)
    | h > 0 = (h + somaPos, produtoNeg)
    | h < 0 = (somaPos, h * produtoNeg)
    | otherwise = (somaPos, produtoNeg)
    where
        (somaPos, produtoNeg) = somaEProduto tail

--2) Conta qtde de pares e qtde de impares
paresEImpares :: [Int] -> (Int, Int)
paresEImpares [] = (0, 0)
paresEImpares (head:tail)
    | head `mod` 2 == 0 = (pares + 1, impares)
    | otherwise = (pares, 1 + impares)
    where 
        (pares, impares) = paresEImpares tail

--3) Retorna a soma e a media dos elementos de uma lista
somaEMedia :: [Float] -> (Float, Float)
somaEMedia [] = (0, 0)
somaEMedia lista = (soma lista, media lista)
    where
        soma [] = 0
        soma (x:xs) = x + soma xs
        media xs = soma xs / fromIntegral (conta xs)
        conta [] = 0
        conta (_:xs) = 1 + conta xs

--4) Separa uma lista de inteiros em duas listas: positivos e negativos
posENeg :: [Int] -> ([Int], [Int])
posENeg [] = ([], [])
posENeg (head:tail)
    | head >= 0 = (head:positivos, negativos)
    | otherwise = (positivos, head:negativos)
    where
        (positivos, negativos) = posENeg tail