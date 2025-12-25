-- 1. 

-- a. Dado un número devuelve su sucesor.
sucesor :: Int -> Int 
sucesor x = x + 1  

-- b.
-- Dados dos números devuelve la suma utilizando la operación '+'. 
sumar :: Int -> Int -> Int 
sumar x y = x + y 

-- c. 
-- Dados dos números devuelve el mayor de ellos. 
maximo :: Int -> Int -> Int 
maximo x y | x >= y    = x 
           | otherwise = y

-- 2. 

-- a. 
-- Dado un booleano, si es True devuelve False, y si es False, devuelve True.
negar :: Bool -> Bool 
negar True  = False 
negar False = True

-- b. 
-- Dados dos booleanos, si ambos son True, devuelve True; sino, devuelve False. 
andLogico :: Bool -> Bool -> Bool 
andLogico True True = True 
andLogico _ _       = False 

-- c. 
-- Dados dos booleanos, si alguno es True, devuelve True; sino, devuelve False. 
orLogico :: Bool -> Bool -> Bool 
orLogico True _ = True 
orLogico _ True = True 
orLogico _ _    = False

-- d. 
-- Dado un par de números devuelve la primera componente. 
primera :: (Int, Int) -> Int 
primera (x, _) = x 

-- e. 
-- Dados un par de números devuelve la segunda componente. 
segunda :: (Int, Int) -> Int 
segunda (_, y) = y 

-- f. 
-- Dado un par de números, devuelve su suma. 
sumaPar :: (Int, Int) -> Int 
sumaPar (x, y) = x + y 

-- g. 
-- Dado un par de números devuelve el mayor de ellos. 
maxDelPar :: (Int, Int) -> Int 
maxDelPar (x, y) = maximo x y 

-- 3. 

-- a.
-- Dado un elemento de algún tipo, devuelve ese mismo elemento. 
loMismo :: a -> a 
loMismo x = x 

-- b. 
-- Dado un elemento de algún tipo, devuelve el número 7. 
siempreSiete :: a -> Int 
siempreSiete _ = 7 

-- c. 
-- Dado un elemento de algún tipo, devuelve un par con ese elemento en ambas
-- componentes. 
duplicar :: a -> (a, a) 
duplicar x = (x, x) 

-- d. 
-- Dado un elemento de algún tipo devuelve una lista con este único elemento. 
singleton :: a -> [a] 
singleton x = [x] 

-- 4. 
-- a. 
-- Dada una lista de elementos, si es vacía devuelve True; sino, devuelve False. 
isEmpty :: [a] -> Bool 
isEmpty [] = True 
isEmpty _  = False

-- b. 
-- Dada una lista devuelve su primer elemento. 
-- Pre: La lista no debe ser vacía. 
head' :: [a] -> a 
head' (x : _) = x

-- c. 
-- Dada una lista devuelve esa lista menor el primer elemento. 
-- Pre: La lista no debe ser vacía. 
tail' :: [a] -> [a] 
tail' (_: xs) = xs  