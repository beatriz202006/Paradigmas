module Tuplas where

-- 1) Funcao que retorna a tupla (2x, 3x, 4x, 5x) dado um inteiro x
tuplaMult :: Int -> (Int, Int, Int, Int)
tuplaMult x = (2 * x, 3 * x, 4 * x, 5 * x)

-- 2) Funcao que, dado um inteiro n, retorna ( n div 2, "Par"/ "Impar")
tuplaParImpar :: Int -> (Int, String)
tuplaParImpar n
    | mod n 2 == 0 = (n `div` 2, "Par")
    | otherwise = (n `div` 2, "Impar")

--3) Funcao recursiva que recebe lista de inteiros e retorna (sosma dos positivos, produto dos negativos)
posNeg :: [Int] -> (Int, Int)
posNeg [] = (0, 0)
posNeg (head:tail)
    | head > 0 = (head + somaPos, prodNeg) -- se x eh positivo
    | head < 0 = (somaPos, head * prodNeg)    -- se x eh negativo
    | otherwise = (somaPos, prodNeg)      -- se x eh zero
    where (somaPos, prodNeg) = posNeg tail


--4) Funcao que recebe lista [(Ponto turistico, avaliacao, tipo)] e retorna lista [(Nome, avaliacao)] para avaliacao >= fornecida pelo usuario
pontosTuristicos :: [(String, Float, String)] -> Float -> [(String, Float)]
pontosTuristicos [] _ = []
pontosTuristicos ((nome, avaliacao, tipo):tail) notaMin
    | avaliacao >= notaMin = (nome, avaliacao) : pontosTuristicos tail notaMin
    | otherwise = pontosTuristicos tail notaMin

--5) Segunda versao usando lista por compreesao e type
type Nome = String
type Avaliacao = Int
type Tipo = String
type PontoTuristico = (Nome, Avaliacao, Tipo)

pontosTuristicos2 :: [PontoTuristico] -> Avaliacao -> [(Nome, Avaliacao)]
pontosTuristicos2 lista notaMin =
    [(nome, avaliacao) | (nome, avaliacao, tipo) <- lista, avaliacao >= notaMin, tipo == "parque" || tipo == "museu"]

    