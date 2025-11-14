module Listas where

--1) Funcao recursiva que soma todos os elementos de uma lista de inteiros
somaInteiros :: [Int] -> Int
somaInteiros [] = 0
somaInteiros (head:tail) = head + somaInteiros tail

--2) Funcao recursiva que remove todos os elementos pares de uma lista de inteiros
removePares :: [Int] -> [Int]
removePares [] = []
removePares (head:tail)
    | even head = removePares tail
    | otherwise = head : removePares tail

--3) Funcao recursiva que inverte os elementos de uma lista de numeros reais
inverteLista :: [Float] -> [Float]
inverteLista [] = []
inverteLista (head:tail) = inverteLista tail ++ [head]

--4) Funcao que retorna lista com os nomes que comecam com a letra 'A' - com Prelude
nomesComA :: [String] -> [String]
nomesComA [] = []
nomesComA (h:t)
    | head h == 'A'  = h : nomesComA t
    | otherwise = nomesComA t

--5) Funcao que retorna lista com os nomes que comecam com a letra 'A' - sem Prelude
nomesComA2 :: [String] -> [String]
nomesComA2 [] = []
nomesComA2 ((a:as):t)
    | a == 'A' = (a:as) : nomesComA2 t
    | otherwise = nomesComA2 t

--6) Lista por compreensao: [0, 3, 6, 9, 12, 15]
listaCompreensao1 :: [Int]
listaCompreensao1 = [x * 3 | x <- [0..5]]

--7) Lista por compreensao [[1], [2], [3], [4], [5]]
listaCompreensao2 :: [[Int]]
listaCompreensao2 = [[x] | x <- [1 .. 5]]
