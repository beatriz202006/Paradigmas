module Bloco1 where

--1) Verifica se numero eh par e positivo
parEPositivo :: Int -> Bool
parEPositivo n
    | n `mod` 2 == 0 && n > 0 = True
    | otherwise = False

--2) Funcao que classifica numero como positivo/negativo/nulo
classifica :: Int -> String
classifica n
    | n > 0 = "Positivo"
    | n < 0 = "Negativo"
    | otherwise = "Zero"

--3) Funcao que recebe duas notas e retorna a situacao do aluno
situacao :: Float -> Float -> String
situacao n1 n2 
    | media >= 7 = "Aprovado"
    | 4 <= media && media <= 6.9 = "Exame"
    | otherwise = "Reprovado"

    where 
        media = (n1 + n2) / 2

--4) Funcao que retorna dois numeros e retorna o maior deles
maiorNumero :: Int -> Int -> Int
maiorNumero n1 n2
    | n1 > n2 = n1
    | otherwise = n2