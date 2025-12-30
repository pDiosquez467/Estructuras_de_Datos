module Stack (
    Stack, 
    emptyS, 
    isEmptyS, 
    push, 
    top, 
    pop
) where

---- Interfaz del módulo Stack
--

-- Propósito:
-- Crea una pila vacía.
-- Complejidad: O(1).
--
emptyS :: Stack a

-- Propósito:
-- Dada una pila, indica si está vacía.
-- Complejidad: O(1).
--
isEmptyS :: Stack a -> Bool

-- Propósito:
-- Dados un elemento y una pila, agrega el elemento a la pila.
-- Complejidad: O(1).
--
push :: a -> Stack a -> Stack a

-- Propósito:
-- Dada una pila devuelve el elemento del tope de la pila.
-- Pre:
-- La pila no debe estar vacía.
-- Complejidad: O(1).
--
top :: Stack a -> a


-- Propósito:
-- Dada una pila devuelve la pila sin el primer elemento.
-- Pre: 
-- La pila no debe estar vacía.
-- Complejidad: O(1).
--
pop :: Stack a -> Stack a 

---- Implementación del módulo Stack
--

-- Usamos 'newtype' en lugar de 'data' porque solo envolvemos un tipo.
-- Es más eficiente en tiempo de ejecución.
newtype Stack a = MkS [a] deriving Show

emptyS            = MkS []

isEmptyS (MkS xs) = null xs

push x (MkS xs)   = MkS (x : xs)

top (MkS xs)      = head xs 

pop (MkS xs)      = MkS (tail xs)
