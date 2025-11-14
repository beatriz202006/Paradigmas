module Exerciciosfinais2 where

--1
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (head:tail) = head + somaLista tail

--2
contaElementos :: [Int] -> Int
contaElementos [] = 0
contaElementos (head:tail) = 1 + contaElementos tail

--3
produtoElementos :: [Int] -> Int
produtoElementos [] = 0
produtoElementos (head:tail) = head * produtoElementos tail

--4
contaMaiores :: [Int] -> Int -> Int
contaMaiores [] _ = 0
contaMaiores (head:tail) x
    | head > x = 1 + contaMaiores tail x
    | otherwise = contaMaiores tail x

--5 -> sem listas por compreensao
filtraPositivos :: [Int] -> [Int]
filtraPositivos [] = []
filtraPositivos (head:tail) 
    | head > 0 = head : filtraPositivos tail
    | otherwise = filtraPositivos tail

--6 -> com listas por compreensao
filtraPositivos2 :: [Int] -> [Int]
filtraPositivos2 lista = [x | x <- lista , x > 0]

--7 
dobraElementos :: [Int] -> [Int]
dobraElementos []  = []
dobraElementos (head:tail) = (head * 2) : dobraElementos tail

--8
removeTudo :: [Int] -> Int -> [Int]
removeTudo [] _ = []
removeTudo (head:tail) n
    | head == n = removeTudo tail n
    | otherwise = head : removeTudo tail n

--9
maiorLista :: [Int] -> Int
maiorLista [x] = x
maiorLista (head:tail)
    | head > maiorTail  = head
    | otherwise = maiorTail
    where
        maiorTail = maiorLista tail

--10
menorLista :: [Int] -> Int
menorLista [x] = x
menorLista [] = 0
menorLista (head:tail) 
    | head < menorTail = head
    | otherwise = menorTail
    where
        menorTail = menorLista tail

--11
verificaLista :: [Int] -> Int -> Bool
verificaLista [] _ = False
verificaLista (head:tail) numero
    | head == numero = True
    | otherwise = verificaLista tail numero

--12:
contaAparicoes :: [Int] -> Int -> Int
contaAparicoes [] _ = 0
contaAparicoes (head:tail) numero
    | head == numero = 1 +  contaAparicoes tail numero
    | otherwise = contaAparicoes tail numero

--13:
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (head:tail) = inverteLista tail ++ [head]