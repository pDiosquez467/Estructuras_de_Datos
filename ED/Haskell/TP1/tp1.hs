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

-- 2. RECURSIÓN

-- 2.1 Recursión sobre listas 
-- a. 
-- Dada una lista de enteros, devuelve la suma de todos los elementos. 
sumatoria :: [Int] -> Int
-- sumatoria [] = 0 
-- sumatoria (x : xs) = x + sumatoria xs 
sumatoria = sum

-- b. 
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la 
-- cantidad de elementos que posee.
longitud :: [a] -> Int
longitud []       = 0
longitud (_ : xs) = 1 + longitud xs

-- c. 
-- Dada una lista de números enteros, devuelve un número que es el promedio entre
-- todos los elementos de la lista. 
promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (longitud xs)

-- d. 
-- Dada una lista de enteros, devuelve la lista de sucesores de cada entero. 
mapSucesor :: [Int] -> [Int]
-- mapSucesor [] = [] 
-- mapSucesor (x : xs) = (sucesor x) : (mapSucesor xs)  
mapSucesor = map sucesor

-- e. 
-- Dada una lista de pares de enteros, devuelve una nueva lista en la que cada elemento
-- es la suma de los elementos de cada par. 
mapSumaPar :: [(Int, Int)] -> [Int]
-- mapSumaPar [] = []
-- mapSumaPar (p : ps) = (sumaPar p) : (mapSumaPar ps) 
mapSumaPar = map sumaPar

-- f. 
-- Dada una lista de pares de enteros, devuelve una nueva lista en la que cada elemento
-- es el mayor de las componentes de cada par. 
mapMaxDelPar :: [(Int, Int)] -> [Int]
-- mapMaxDelPar [] = []
-- mapMaxDelPar (p : ps) = (maxDelPar p) : (mapMaxDelPar ps)
mapMaxDelPar = map maxDelPar


-- g. 
-- Dada una lista de booleanos devuelve True si todos los elementos son True. 
todoVerdad :: [Bool] -> Bool
-- todoVerdad [] = True 
-- todoVerdad (x : xs) = x && todoVerdad xs 
-- todoVerdad = foldr (&&) True
todoVerdad = and

-- h. 
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True. 
algunaVerdad :: [Bool] -> Bool
-- algunaVerdad [] = False 
-- algunaVerdad (x: xs) = x || algunaVerdad xs 
algunaVerdad = foldr (||) True

-- i. 
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea 
-- igual a e. 
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:  xs) = (e == x) || pertenece e xs


-- j. 
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs. 
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x : xs) | e == x    = 1 + apariciones e xs
                       | otherwise = apariciones e xs

-- k.
-- Dados un elemento e y una lista xs devuelve todos los elementos de xs que son menores a e. 
filtrarMenores :: Ord a => a -> [a] -> [a]
filtrarMenores _ []       = []
filtrarMenores e (x : xs) | x < e     = x : filtrarMenores e xs
                          | otherwise = filtrarMenores e xs

-- filtrarMenores' e = filter (< e) 

-- l. 
-- Dados un elemento y una lista, filtra (elimina) todas las ocurrencias de ese elemento en la lista.
filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento _ []       = []
filtrarElemento e (x : xs) | e == x    = filtrarElemento e xs
                           | otherwise = x : filtrarElemento e xs

-- m. 
-- Dada una lista de listas, devuelve la lista de sus longitudes.
mapLongitudes :: [[a]] -> [Int]
-- mapLongitudes = map longitud
mapLongitudes [] = []
mapLongitudes (xs : xss) = longitud xs : mapLongitudes xss

-- n. 
-- Dado un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n 
-- elementos. 
longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA _ []         = []
longitudMayorA n (xs : xss) | longitud xs > n = xs : longitudMayorA n xss
                            | otherwise       = longitudMayorA n xss
-- longitudMayorA' n = filter (> n . longitud) 

-- o. 


-- p. 
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la misma.
snoc :: [a] -> a -> [a]
-- snoc [] e       = [e] 
-- snoc (x : xs) e = x : (snoc xs e) 
snoc xs e = foldr (:) [e] xs

-- q. 
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los de la
-- segunda a continuación. 
append :: [a] -> [a] -> [a]
append = foldl snoc


-- r. 
-- Dada una lista de listas, devuelve una única lista con todos sus elementos. 
aplanar :: [[a]] -> [a] 
-- aplanar []         = [] 
-- aplanar (xs : xss) = append xs (aplanar xss) 
aplanar = foldr append [] 


-- s. 
-- Dada una lista devuelve la lista con todos los elementos de atrás para adelante. 
reversa :: [a] -> [a] 
reversa []       = []
reversa (x : xs) = snoc (reversa xs) x  

-- t. 
-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera y de la segunda lista, teniendo en cuenta que no
-- necesariamente las lista tienen la misma longitud. 
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys             = ys
zipMaximos xs []             = xs 
zipMaximos (x : xs) (y : ys) = maximo x y : zipMaximos xs ys 

-- u.
-- Dadas dos listas de enteros de igual longitud, devuelve una lista de pares '(min, max)', donde
-- 'min' y 'max' son el mínimo y el máximo entre los elementos de ambas listas en la misma
-- posición. 
zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] _ = [] 
zipSort (x : xs) (y : ys) = (minimo x y, maximo x y) : zipSort xs ys        
        where minimo :: Int -> Int -> Int 
              minimo x y | x < y      = x 
                   | otherwise = y  