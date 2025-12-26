-- TIPOS ALGEBRAICOS

-- Tipos Enumerativos
--
data Origen = Animal | Vegetal | Mineral
			deriving Show


-- Propósito: Dada una lista de orígenes, devuelve la cantidad
-- de animales que ésta tiene.
--
nroAnimales :: [Origen] -> Int
nroAnimales []       = 0
nroAnimales (o : os) = if (esAnimal o)
						 then 1 + nroAnimales
						 else nroAnimales

-- Propósito: Indica si el origen dado es Animal.
--
esAnimal :: Origen -> Bool
esAnimal Animal = True
esAnimal _      = False 


-- Tipos producto (registros)
--
-- Un ingrediente cuenta con una descripción, su origen y el
-- valor energético por cada 100 gramos expresado en Kcal:

type Nombre = String
data Ingrediente = MkI Nombre Origen Int
				   deriving Show

-- MkI es una función que construye un 'Ingrediente'.
-- MkI :: Nombre -> Origen -> Int -> Ingrediente
-- Es común ver: data Foo = MkFoo Int String ...

bollo  :: Ingrediente
bollo  = MkI "Masa de pizza" Vegetal 228

tomate :: Ingrediente
tomate = MkI "Tomate" Vegetal 22

queso  :: Ingrediente
queso  = MkI "Queso muzzarella" Animal 245


-- Funciones proyectoras (u observadoras)
--

descripcionI :: Ingrediente -> Nombre
descripcionI (MkI nombre _ _) = nombre

origenI :: Ingrediente -> Origen
origenI (MkI _ origen _) = origen

valorEnergeticoI :: Ingrediente -> Int
valorEnergeticoI (MkI _ _ calorias) = calorias 
