module Exercicios where

--1) Funcao que recebe uma nota e retorna se foi aprovado/reprovado/recuperacao
avaliaNota :: Float -> String
avaliaNota nota 
    | nota >= 7.0 = "Aprovado"
    | 4.0 <= nota && nota <= 6.9 = "Exame"
    | otherwise = "Reprovado"

--2) Funcao que recebe uma lista de inteiros e retorna quantos sao pares
contaPares :: [Int] -> Int
contaPares [] = 0
contaPares (head:tail)
    | head `mod` 2 == 0 = 1 + contaPares tail
    | otherwise = contaPares tail

--3) Funcao que recebe uma tupla (String, Int, Char) e retorna o nome
dadosPessoa :: (String, Int, Char) -> String
dadosPessoa (nome, _, _) = "Nome: " ++ nome


--4) 
type Pesquisador = (String, String, Char)
base :: Int -> Pesquisador
base x
    | x == 1 = ("Joao", "mestre", 'm')
    | x == 2 = ("Jonas", "doutor", 'm')
    | x == 3 = ("Joice", "mestre", 'f')
    | x == 4 = ("Janete", "doutor", 'f')
    | x == 5 = ("Jocileide", "doutor", 'f')
    | otherwise = ("Ninguem", "nenhum", 'x')

grupo :: Int -> [Pesquisador]
grupo 0 = []
grupo n = grupo (n-1) ++ [base n]

--5) Funcao que recebe uma lista de pesquisadores e retorna apenas os nomes das mulheres doutoras
mulheresDoutoras :: [Pesquisador] -> [String]
mulheresDoutoras lista = [nome | (nome, titulacao, genero) <- lista, titulacao == "doutor" && genero == 'f']

somaEConta :: [Float] -> (Float, Int)
somaEConta [] = (0, 0)
somaEConta (head:tail) = (head + somaT, 1 + qtde)
    where
        (somaT, qtde) = somaEConta tail