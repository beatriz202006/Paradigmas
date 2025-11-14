module Exerciciosfinais3 where

--1a
filtraNomes1 :: [String] -> Char -> [String]
filtraNomes1 [] _ = []
filtraNomes1 ((head:tail) : t) letra
    | head == letra = (head:tail) : filtraNomes1 t letra
    | otherwise = filtraNomes1 t letra

filtraNomes2 :: [String] -> Char -> [String]
filtraNomes2  [] _ = []
filtraNomes2 lista letra= [(head:tail) | (head:tail) <- lista, head == letra]

--2a
filtraPares1 :: [Int] -> [Int]
filtraPares1 [] = []
filtraPares1 (head:tail) 
    | head `mod` 2 == 0 = head : filtraPares1 tail
    | otherwise = filtraPares1 tail

--2b
filtraPares2 :: [Int] -> [Int]
filtraPares2 [] = []
filtraPares2 lista = [x | x <- lista, x `mod` 2 == 0]

--3a
somaMaiores1 :: [Int] -> Int -> Int
somaMaiores1 [] _ = 0
somaMaiores1 (head:tail) limite
    | head > limite = head + somaMaiores1 tail limite
    | otherwise = somaMaiores1 tail limite

--3b
somaMaiores2 :: [Int] -> Int -> Int
somaMaiores2 [] _ = 0
somaMaiores2 lista limite  = sum [x |  x <- lista, x > limite]

--4a
extrairSegundos :: [(Int, Int)] -> [Int]
extrairSegundos [] = []
extrairSegundos ((_, n2) : tail) = n2 : extrairSegundos tail

--4b
extrairSegundos2 :: [(Int, Int)] -> [Int]
extrairSegundos2 [] = []
extrairSegundos2 lista = [x | (_, x) <- lista]
