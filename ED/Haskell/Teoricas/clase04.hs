-- ============================================================================
-- Árboles binarios
-- ============================================================================
-- Definición del tipo Arbol y funciones básicas definidas mediante
-- recursión estructural.
-- ============================================================================

-- Árbol binario sin información en los nodos.
data Arbol
    = Nil
    | Bin Arbol Arbol
    deriving Show

-- ============================================================================
-- Funciones auxiliares
-- ============================================================================

-- Indica si un árbol es vacío.
esNil :: Arbol -> Bool
esNil Nil = True
esNil _   = False

-- Devuelve el subárbol izquierdo.
-- Precondición: el árbol no es Nil.
hijoIzq :: Arbol -> Arbol
hijoIzq (Bin i _) = i

-- Devuelve el subárbol derecho.
-- Precondición: el árbol no es Nil.
hijoDer :: Arbol -> Arbol
hijoDer (Bin _ d) = d

-- ============================================================================
-- Funciones definidas por recursión estructural
-- ============================================================================

-- Devuelve la cantidad de nodos del árbol.
cantNodos :: Arbol -> Int
cantNodos Nil       = 0
cantNodos (Bin i d) = 1 + cantNodos i + cantNodos d

-- Devuelve la cantidad de hojas del árbol.
-- Una hoja es un nodo cuyos hijos son Nil.
cantHojas :: Arbol -> Int
cantHojas Nil = 0
cantHojas (Bin i d)
    | esNil i && esNil d = 1
    | otherwise         = cantHojas i + cantHojas d

-- Devuelve la altura del árbol.
-- La altura del árbol vacío es 0.
altura :: Arbol -> Int
altura Nil       = 0
altura (Bin i d) = 1 + max (altura i) (altura d)
