{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Cola where

---- Interfaz del módulo Cola

vaciaC      :: Cola a
estaVaciaC  :: Cola a -> Bool
encolarC    :: a -> Cola a -> Cola a

-- Pre: La cola no debe estar vacía. 
--
proximoC    :: Cola a -> a

-- Pre: La cola no debe estar vacía. 
--
desencolarC :: Cola a -> Cola a

---- Implementación del módulo Cola 
----------------------------------------------------------------
-- Variante 1: el próximo elemento elemento se encuentra al principio
-- de la lista. 
--

-- estaVaciaC: es O(1).

-- encolarC: es lineal, o sea O(n) para una cola de n elementos. 

-- proximoC: es O(1).

-- desencolarC: es O(1).

-- data Cola a = MkC [a]

-- vaciaC               = MkC [] 

-- estaVaciaC (MkC xs)  = null xs

-- encolarC x (MkC xs)  = MkC (xs ++ [x])

-- proximoC (MkC xs)    = head xs 

-- desencolarC (MkC xs) = MkC (tail xs)

----------------------------------------------------------------
-- Variante 2: El próximo elemento se encuentra al final de la lista. 
--

-- estaVaciaC: es O(1).

-- encolarC: es O(1). 

-- proximoC: es O(n) siendo n la cantidad de elementos de la cola.

-- desencolarC: es O(n), siendo n la cantidad de elementos de la cola.

-- data Cola a = MkC [a]

-- vaciaC = MkC []

-- estaVaciaC (MkC xs)  = null xs 

-- encolarC x (MkC xs)  = MkC (x : xs)

-- proximoC (MkC xs)    = last' xs         -- last: es lineal. 

-- desencolarC (MkC xs) = MkC (init' xs)   -- init: es lineal. 

-- -- Propósito: 
-- -- Devuelve el último elemento de la lista. 
-- -- Pre: La lista no debe ser vacía.
-- -- Complejidad: O(n), pues debe recorrer toda la lista hasta 
-- -- alcanzar el último.
-- --
-- last' :: [a] -> a 
-- last' (x : xs) = 
--     if null xs 
--         then x 
--         else last' xs

-- -- Propósito: 
-- -- Devuelve una nueva lista con todos los elementos de la lista dada 
-- -- excepto el último. 
-- -- Pre: La lista no debe ser vacía.
-- -- Complejidad: O(n), pues debe recorrer toda la lista hasta 
-- -- alcanzar el último.
-- --
-- init' :: [a] -> [a] 
-- init' (x : xs) = 
--     if null xs 
--         then []
--         else init' xs 

----------------------------------------------------------------
-- Variante 3:
