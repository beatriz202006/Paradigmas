module Tuplas4 where

--1)a) Funcao que recebe um valor real e verifica se eh positivo
verificaPositivo :: Float -> Bool
verificaPositivo n
    | n > 0 = True
    | otherwise = False

--b) Funcao que recebe um valor real e verifica se eh negativo
verificaNegativo :: Float -> Bool
verificaNegativo n
    | n < 0 = True
    | otherwise = False

--c) Funcao que recebe um valor real e verifica se eh zero
verificaZero :: Float -> Bool
verificaZero n
    | n == 0 = True
    | otherwise = False

--2) Funcao que recebe uma lista de valores reais e verifica a condicao
filtra :: [Float] -> (Float->Bool) -> [Float]
filtra [] _ = []
filtra (head:tail) condicao 
    | condicao head = head : filtra tail condicao
    | otherwise = filtra tail condicao

numeros :: [Float]
numeros = [10, -3, 0, 7, -2, 4, 0, -8]