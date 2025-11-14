module Tuplas2 where

--1a) Funcao que recebe uma tupla do tipo (String, String, Char) e retorna o primeiro elemento
primeiroElemento :: (String, String, Char) -> String
primeiroElemento (a, _, _) = a

--1b) Funcao que recebe uma tupla do tipo (Int, Int) e retorna o segundo elemento
segundoElemento :: (String, String, Char) -> String
segundoElemento (_, b, _) = b

--1c) Funcao que recebe uma tupla do tipo (Float, Float, Float) e retorna o terceiro elemento
terceiroElemento :: (String, String, Char) -> Char
terceiroElemento (_, _, c) = c

--2)
base :: Int -> (String, String, Char)
base x
    |x == 1 = ("joao", "mestre", 'm')
    |x == 2 = ("jonas", "doutor", 'm')
    |x == 3 = ("joice", "mestre", 'f')
    |x == 4 = ("janete", "doutor", 'f')
    |x == 5 = ("jocileide", "doutor", 'f')
    |otherwise = ("ninguem", "nada", 'x')

--a) Funcao recursiva que retorna o numero de mestres
contMestre :: Int -> Int
contMestre 0 = 0
contMestre x 
    | segundoElemento (base x) == "mestre" = 1 + contMestre(x-1)
    | otherwise = contMestre(x-1)

--b) Funcao recursiva que conta o numero de doutores
contDoutor :: Int -> Int
contDoutor 0 = 0
contDoutor x 
    | segundoElemento (base x) == "doutor" = 1 + contDoutor(x-1)
    | otherwise = contDoutor(x-1)

--c) versao generica contMD
contMD :: Int -> String -> Int
contMD 0 _ = 0
contMD x titulacao
    | segundoElemento (base x) == titulacao = 1 + contMD(x-1) titulacao
    | otherwise = contMD(x-1) titulacao

-- d) versao generica que tambem recebe genero como parametro
contMDG :: Int -> String -> Char -> Int
contMDG 0 _ _ = 0
contMDG x titulacao genero
    | segundoElemento (base x) == titulacao && terceiroElemento (base x) == genero = 1 + contMDG(x-1) titulacao genero
    | otherwise = contMDG(x-1) titulacao genero


--3)a) 
type Nome = String
type Titulo = String
type Genero = Char
type Pesquisador = (Nome, Titulo, Genero)
type Grupo = [Pesquisador]

--b) funcao recursiva que forma uma lista de pesquisadores com os elementos da funcao base
listaPesq :: Int -> Grupo
listaPesq 0 = []
listaPesq x = base x : listaPesq (x-1)

--c) Funcao que recebe lista de pesquisadores e retorna lista com o nome dos pesquisadores com titulo de doutor
listaDoutores :: Grupo -> [Nome]
listaDoutores [] = []
listaDoutores ((nome, titulo, genero):tail)
    | titulo == "doutor" = nome : listaDoutores tail
    | otherwise = listaDoutores tail

--d) Segunda versao usando lista por compreensao
listaDoutores2 :: Grupo -> [Nome]
listaDoutores2 grupo = [ n | (n, t, _) <- grupo, t == "doutor"]

