module Bloco5 where

--1) Cria lista [0, 3, 6, 9, 12, 15]
lista1 :: [Int]
lista1 = [x*3 | x <- [0..5]]

--2) Lista de listas [[1],[2],[3],[4],[5]]
lista2 :: [[Int]]
lista2 = [[n] | n <- [1..5]]

--3) Filtra nomes que comecam com a letra A
filtraA :: [String] -> [String]
filtraA nomes = [n | n <- nomes, head n == 'A']

--4) Versao 2 => sem Prelude
filtraA2 :: [String] -> [String]
filtraA2 [] = []
filtraA2 ((head:tail):t)
    | head == 'A' = (head:tail) : filtraA2 t
    | otherwise = filtraA2 t 

--5) Cria apelidos de tipo com type
type Nome = String
type Nota = Float
type Tipo = String
type PontoTuristico = (Nome, Nota, Tipo)

pontos :: [PontoTuristico]
pontos = [("Barigui", 10, "parque"),
          ("Oscar Niemeyer", 9, "museu"),
          ("Rua XV", 7, "comercio"),
          ("Tanguá", 8.5, "parque")]

-- Exemplo de teste:
-- filtraPontos pontos 8
-- Resultado: [("Barigui",10.0),("Oscar Niemeyer",9.0),("Tanguá",8.5)]

filtraPontos :: [PontoTuristico] -> Nota -> [(Nome, Nota)]
filtraPontos lista av = [(n, nota) | (n, nota, tipo) <- lista, nota > av && (tipo == "parque" || tipo == "museu")]
