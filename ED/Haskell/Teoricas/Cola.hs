module Cola 
    ( Cola
    , vaciaC       -- :: Cola a
    , estaVaciaC   -- :: Cola a -> Bool
    , encolarC     -- :: a -> Cola a -> Cola a
    , proximoC     -- :: Cola a -> a
    , desencolarC  -- :: Cola a -> Cola a
    ) where

-------------------------------------------------------------------------------
-- INTERFAZ
-------------------------------------------------------------------------------

-- | Tipo abstracto Cola (FIFO).
data Cola a = MkC [a] [a] deriving Show

vaciaC      :: Cola a
estaVaciaC  :: Cola a -> Bool
encolarC    :: a -> Cola a -> Cola a

-- | Devuelve el próximo elemento (el primero en haber entrado).
-- Pre: La cola no debe estar vacía.
proximoC    :: Cola a -> a

-- | Elimina el primer elemento de la cola.
-- Pre: La cola no debe estar vacía.
desencolarC :: Cola a -> Cola a

-------------------------------------------------------------------------------
-- IMPLEMENTACIÓN (Variante 3: Dos Listas)
-------------------------------------------------------------------------------
-- Estructura: MkC front back
-- Invariante: Los elementos se encolan en 'back' y se sacan de 'front'.
-- Cuando 'front' se vacía, se invierte 'back' y se pasa a 'front'.

-- Costo: O(1)
vaciaC = MkC [] []

-- Costo: O(1)
estaVaciaC (MkC fs bs) = null fs && null bs

-- Costo: O(1)
encolarC x (MkC fs bs) = MkC fs (x : bs)

-- Costo: O(1) si fs no es vacía. O(n) en el peor caso (si fs es vacía).
proximoC (MkC fs bs) =
    if null fs
        then last bs 
        else head fs 

-- Costo: O(1) amortizado.
desencolarC (MkC fs bs) =
    if null fs
        then MkC (tail (reverse' bs)) []  -- O(n) al invertir, pero ocurre poco.
        else MkC (tail fs) bs             -- O(1)

-------------------------------------------------------------------------------
-- AUXILIARES
-------------------------------------------------------------------------------

-- Propósito: Invierte una lista.
-- Complejidad: O(n).
reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []

reverse'' :: [a] -> [a] -> [a]
reverse'' xs ys = foldl (flip (:)) ys xs

-------------------------------------------------------------------------------
-- OTRAS VARIANTES (NO UTILIZADAS)
-------------------------------------------------------------------------------

{- 
-- Variante 1: El próximo elemento está al PRINCIPIO de la lista.
-- Costos: encolar O(n), resto O(1).

data Cola a = MkC [a]

vaciaC               = MkC [] 
estaVaciaC (MkC xs)  = null xs
encolarC x (MkC xs)  = MkC (xs ++ [x])
proximoC (MkC xs)    = head xs 
desencolarC (MkC xs) = MkC (tail xs)
-}

{- 
-- Variante 2: El próximo elemento está al FINAL de la lista.
-- Costos: encolar O(1), proximo/desencolar O(n).

data Cola a = MkC [a]

vaciaC               = MkC []
estaVaciaC (MkC xs)  = null xs 
encolarC x (MkC xs)  = MkC (x : xs)
proximoC (MkC xs)    = last' xs         
desencolarC (MkC xs) = MkC (init' xs)   

-- Auxiliar: last'
-- Complejidad: O(n).
last' :: [a] -> a 
last' (x : xs) = if null xs then x else last' xs

-- Auxiliar: init'
-- Complejidad: O(n).
init' :: [a] -> [a] 
init' (x : xs) = 
    if null xs 
        then []
        else x : init' xs  -- Corrección: se debe conservar x.
-}