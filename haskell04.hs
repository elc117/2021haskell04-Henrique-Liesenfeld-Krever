-- Prática 04 de Haskell
-- Nome: Henrique Liesenfeld Krever


--1. verificar quem tomar a vacina a partir da idade
faixaIdoso :: Int -> String
faixaIdoso idade | idade > 79 = "IDO80"
 | idade > 74 = "IDO79"
 | idade > 69 = "IDO74"
 | idade > 64 = "IDO69"
 | idade > 59 = "IDO64"
 | otherwise = "ND"

--2. verificar idade da pessoa list comprehension
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos tupla = [(x,y,faixaIdoso y) | (x,y) <- tupla]

--3. mesma coisa que a anterior
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' tupla = map (\(str, idade) -> (str, idade, faixaIdoso idade)) tupla

--4 tranforma tupla de int em string
strColor :: (Int,Int,Int) -> String
strColor tupla = "rgb"++ show tupla

--5 circulos circulos e mais circulos
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = [(x, cy, r) | x <- [cx, cx+2*r..cx+2*r*(n-1)]]

--6 gera tons de vermelho
genReds :: Int -> [(Int,Int,Int)]
genReds x = [(r,0,0) | r <- [50, 60.. if 50+10*x >255 then 255 else 50+10*x]]