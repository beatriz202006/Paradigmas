module Bloco2 where

--1) Soma dos elementos de uma lista de inteiros
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (head:tail) = head + somaLista tail

--2) Funcao que retorna qtde de elementos de uma lista
contaElementos :: [Int] -> Int
contaElementos [] = 0
contaElementos (head:tail) = 1 + contaElementos tail

--3) Funcao que retorna o maior elemento de uma lista
maiorLista :: [Int] -> Int
maiorLista [x] = x
maiorLista (head:tail) 
    | head > maiorLista tail = head
    | otherwise = maiorLista tail

--4) Funcao que verifica se um numero esta presente na lista
verificaNumero :: [Int] -> Int ->  Bool
verificaNumero [] _ = False
verificaNumero (head:tail) n
    | head == n = True
    | otherwise = verificaNumero tail n

--5) Funcao que inverte uma lista recursivamente
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (head:tail) = inverteLista tail ++ [head]

--6) Conta quantos pares tem na lista
contaPares :: [Int] -> Int
contaPares [] = 0
contaPares (head:tail)
    | head `mod` 2 == 0 = 1 + contaPares tail
    | otherwise = contaPares tail

--7) Remove todos os numeros negativos de uma lista
removeNegativos :: [Int] -> [Int]
removeNegativos [] = []
removeNegativos (head:tail)
    | head > 0 = head : removeNegativos tail
    | otherwise = removeNegativos tail