-- TP3: ÁRBOLES BINARIOS
--

data Tree a = Nil
            | Node a (Tree a) (Tree a)
            deriving Show

-- Propósito:
-- Dada un árbol binario de enteros devuelve la suma entre
-- sus elementos.
sumarT :: Tree Int -> Int
sumarT Nil              = 0
sumarT (Node x izq der) = x + sumarT izq + sumarT der

-- Propósito:
-- Dado un árbol binario devuelve su cantidad de elementos, es
-- decir, el tamaño del árbol (size, en inglés).
sizeT :: Tree a -> Int
sizeT Nil              = 0
sizeT (Node _ izq der) = 1 + sizeT izq + sizeT der

-- Propósito:
-- Dado un árbol de enteros devuelve un árbol con el doble de 
-- cada número. 
mapDobleT :: Tree Int -> Tree Int
mapDobleT Nil              = Nil
mapDobleT (Node x izq der) = Node (2 * x) (mapDobleT izq) (mapDobleT der) 

-- Propósito:
-- Dada una función de transformación y un árbol binario, aplica
-- la función a cada nodo del árbol manteniendo la estructura
-- original.
mapT :: (a -> b) -> Tree a -> Tree b
mapT _ Nil              = Nil
mapT f (Node x izq der) = Node (f x) (mapT f izq) (mapT f der) 

-- Definición de Direcciones
data Dir = Norte | Este | Sur | Oeste deriving Show

-- Propósito:
-- Dado un árbol de direcciones t devuelve un árbol con la 
-- dirección opuesta para cada elemento de t.
mapOpuestoT :: Tree Dir -> Tree Dir
mapOpuestoT = mapT opuesto
  where opuesto :: Dir -> Dir
        opuesto Norte = Sur 
        opuesto Este  = Oeste 
        opuesto Sur   = Norte 
        opuesto Oeste = Este 

-- Propósito:
-- Dado un árbol de palabras devuelve un árbol con la longitud
-- de cada palabra.
mapLongitudT :: Tree String -> Tree Int
mapLongitudT = mapT length

-- Propósito:
-- Dados un elemento y un árbol binario devuelve True si existe
-- un elemento igual al dado en el árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ Nil              = False
perteneceT e (Node x izq der) = e == x || perteneceT e izq || perteneceT e der

-- Propósito:
-- Dados un elemento e y un árbol binario devuelve la cantidad
-- de elementos del árbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ Nil              = 0
aparicionesT e (Node x izq der) =
    if e == x
        then 1 + aparicionesT e izq + aparicionesT e der
        else aparicionesT e izq + aparicionesT e der

-- Propósito:
-- Dados dos árboles construye un árbol t en el que ambos árboles
-- son hijos de t, y en la raíz de t se guarda la suma de todos 
-- los elementos de los hijos de t.
engancharYSumaEnRaiz :: Tree Int -> Tree Int -> Tree Int
engancharYSumaEnRaiz s u = Node (sumarT s + sumarT u) s u

-- Propósito:
-- Dado un árbol devuelve su cantidad de hojas.
-- Nota: una hoja (leaf en inglés) es un nodo que no tiene hijos. 
leaves :: Tree a -> Int
leaves Nil              = 0
leaves (Node _ izq der) =
    if esNil izq && esNil der
        then 1
        else leaves izq + leaves der

-- Propósito:
-- Indica si el árbol dado es Nil.
esNil :: Tree a -> Bool
esNil Nil = True
esNil _   = False 

-- Propósito:
-- Dado un árbol devuelve su altura.
heightT :: Tree a -> Int
heightT Nil              = 0
heightT (Node _ izq der) = 1 + max (heightT izq) (heightT der)

-- Propósito:
-- Dado un árbol devuelve el número de nodos que no son hojas.
nodes :: Tree a -> Int
nodes Nil              = 0
nodes (Node _ izq der) =
    if not (esNil izq) || not (esNil der)
        then 1 + nodes izq + nodes der
        else 0 -- Si es hoja, no suma nada (0 + 0)

-- Propósito:
-- Dado un árbol devuelve el árbol resultante de intercambiar
-- el hijo izquierdo con el derecho en cada nodo del árbol.
espejoT :: Tree a -> Tree a
espejoT Nil              = Nil
espejoT (Node x izq der) = Node x (espejoT der) (espejoT izq)

-- Propósito:
-- Dado un árbol devuelve una lista que representa el resultado
-- de recorrerlo en modo preorder.
listPreOrder :: Tree a -> [a] 
listPreOrder Nil              = []
listPreOrder (Node x izq der) = [x] ++ listPreOrder izq ++ listPreOrder der 

-- Propósito:
-- Recorrido In-Order: Izquierda -> Raíz -> Derecha
listInOrder :: Tree a -> [a]
listInOrder Nil              = []
listInOrder (Node x izq der) = listInOrder izq ++ [x] ++ listInOrder der

-- Propósito:
-- Recorrido Post-Order: Izquierda -> Derecha -> Raíz
listPostOrder :: Tree a -> [a]
listPostOrder Nil              = []
listPostOrder (Node x izq der) = listPostOrder izq ++ listPostOrder der ++ [x] 

-- Propósito:
-- Dado un árbol de listas devuelve la concatenación de todas
-- esas listas. Nota: El recorrido debe ser in-order.
concatenarListasT :: Tree [a] -> [a]
concatenarListasT t = concat (listInOrder t)

-- Propósito:
-- Dados un número n y un árbol devuelve una lista con los nodos
-- de nivel n. Nota: El primero nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN _ Nil              = []
levelN 0 (Node x _ _)     = [x]
levelN n (Node _ izq der) = levelN (n-1) izq ++ levelN (n-1) der

-- Propósito:
-- Dado un árbol devuelve una lista de listas en la que cada
-- elemento representa un nivel de cada árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel Nil = []
listPerLevel t   = [levelN n t | n <- [0 .. heightT t - 1]]

-- Propósito:
-- Dado un árbol devuelve su ancho (width en inglés), que es la 
-- cantidad de nodos del nivel con mayor cantidad de nodos.
widthT :: Tree a -> Int
widthT Nil = 0
widthT t   = maximum (map length (listPerLevel t))
