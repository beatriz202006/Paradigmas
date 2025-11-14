module Tuplas3 where

--1)a) Funcao que recebe um nome e adiciona no inicio o termo "Sr."
adicionaSr :: String -> String
adicionaSr nome = "Sr. " ++ nome

--b) Funcao que recebe um nome e adiciona no inicio o termo "Sra."
adicionaSra :: String -> String
adicionaSra nome = "Sra. " ++ nome

--c) Funcao que recebe um nome e adiciona no inicio o termo "Srta."
adicionaSrta :: String -> String
adicionaSrta nome = "Srta. " ++ nome

--2) Funcao que mapeia o tratamento 
mapearTratamento :: [String] -> (String->String) -> [String]
mapearTratamento [] _ = []
mapearTratamento (head:tail) funcao = funcao head : mapearTratamento tail funcao