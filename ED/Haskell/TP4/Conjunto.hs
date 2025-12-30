module Conjunto where


-- Interfaz del módulo Conjunto
--

-- Crea un conjunto vacío.
--
vacioC :: Conjunto a

-- Propósito:
-- Dados un elemento y un conjunto, agrega el elemento al 
-- conjunto.
--
agregarC :: Eq a => a -> Conjunto a -> Conjunto a

-- Propósito:
-- Dados un elemento y un conjunto indica si el elemento 
-- pertenece al conjunto.
--
perteneceC :: Eq a => a -> Conjunto a -> Bool

-- Propósito:
-- Devuelve la cantidad de elementos distintos de un conjunto.
--
cantidadC :: Eq a => Conjunto a -> Int

-- Propósito:
-- Borra del conjunto dado el elemento dado. Si el elemento 
-- no pertenece al conjunto, deja al conjunto igual.
--
borrarC :: Eq a => a -> Conjunto a -> Conjunto a


-- Propósito:
-- Dados dos conjuntos devuelve un conjunto con todos los elementos
-- de ambos conjuntos.
--
unionC :: Eq a -> Conjunto a -> Conjunto a -> Conjunto a

-- Propósito:
-- Dado un conjunto devuelve una lista con todos los elementos
-- distintos del conjunto.
listaC :: Eq a => Conjunto a -> [a]
