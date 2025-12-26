-- 1.
-- 

data Dir = Norte
    	 | Este
		 | Sur
		 | Oeste
		 deriving Show

-- Propósito: Dada una dirección devuelve su opuesto.
--
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este

-- Propósito: Dada una dirección devuelve su siguiente, en
-- sentido horario.
--
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = Norte

-- 2.
--
type Nombre = String
type Edad   = Int

data Persona = MkP Nombre Edad
			   deriving Show

nombre :: Persona -> Nombre
nombre (MkP n _) = n

edad :: Persona -> Edad
edad (MkP _ e) = e

-- Propósito: Dada una persona la devuelve con su edad aumentada
-- en 1.
-- 
crecer :: Persona -> Persona
crecer persona = MkP (nombre persona) (edad persona + 1)

-- Propósito: Dados un nombre y una persona, reemplaza el
-- nombre de la persona por este otro.
--
cambioDeNombre ::  Nombre -> Persona -> Persona
cambioDeNombre nombre persona = MkP nombre (edad persona)

-- Propósito: Dadas dos personas indica si la primera es más
-- joven que la segunda.
--
esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra persona otra = edad persona < edad otra

-- Propósito: Dados una edad y una lista de personas devuelve
-- todas las personas que son mayores a esa edad.
--
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (p : ps) = if (edad p > n)
						then p : mayoresA n ps
						else mayoresA n ps

-- Propósito: Dada una lista de personas devuelve el promedio
-- de edad entre esas personas.
-- La lista posee al menos una persona.
--
promedioEdad :: [Persona] -> Edad
promedioEdad ps = div (sumaEdades ps) (cantidadDePersonas ps) 

-- Funciones auxiliares
--

sumaEdades :: [Persona] -> Int
sumaEdades [] = 0
sumaEdades (p : ps) = edad p + sumaEdades ps 

cantidadDePersonas :: [Persona] -> Int 
cantidadDePersonas [] = 0
cantidadDePersonas (p : ps) = 1 + cantidadDePersonas ps


-- Propósito: Dada una lista de personas devuelve la persona
-- más vieja de la lista.
-- La lista posee al menos una persona.
--
elMasViejo :: [Persona] -> Persona
elMasViejo (p : ps) = elMasViejo' ps p 

elMasViejo' :: [Persona] -> Persona -> Persona
elMasViejo' [] v       = v
elMasViejo' (p : ps) v = if (edad p > edad v)
							then elMasViejo' ps p
							else elMasViejo' ps v

-- 3.
-- 
data TipoDePokemon = Agua 
				   | Planta 
                   | Fuego
				   deriving Show

type Energia = Int

data Pokemon = MkP TipoDePokemon Energia
			   deriving Show

tipoP :: Pokemon -> TipoDePokemon
tipoP (MkP t _) = t

-- Indica si el pokemon tiene energía para pelear.
--
tieneEnergia :: Pokemon -> Bool
tieneEnergia (MkP _ e) = e > 0

--
data Entrenador = MkE Nombre [Pokemon]
				  deriving Show

pokemonsE :: Entrenador -> [Pokemon]
pokemonsE (MkE _ ps) = ps

-- Devuelve el elemento que le gana a ese.
--
elementoGanador :: TipoDePokemon -> TipoDePokemon
elementoGanador Agua   = Planta 
elementoGanador Planta = Fuego 
elementoGanador Fuego  = Agua


-- Dados dos pokemons indica si el primero le gana al segundo.
-- Se considera que gana si su elemento es opuesto al del otro.
-- Si poseen el mismo elemento se considera que no gana.
--
leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA p otro = esDeTipo (elementoGanador (tipoP otro)) p 

-- Agrega un pokemon a la lista de pokemons del entrenador.
--
capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon p (MkE n ps) = MkE n (p : ps)

-- Devuelve la cantidad de pokemons que posee el entrenador.
--
cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons (MkE _ ps) = length ps

-- Devuelve la cantidad de pokemons de determinado tipo que
-- posee el entrenador.
--
cantidadDePokemonsDeTipo :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonsDeTipo t (MkE _ ps) =
	cantidadDePokemonsDeTipo' t ps 

cantidadDePokemonsDeTipo' :: TipoDePokemon -> [Pokemon] -> Int
cantidadDePokemonsDeTipo' _ [] = 0
cantidadDePokemonsDeTipo' t (p : ps) = 
	if (esDeTipo t p)
	  then 1 + cantidadDePokemonsDeTipo' t ps
	  else     cantidadDePokemonsDeTipo' t ps

esDeTipo Agua (MkP Agua _)     = True 
esDeTipo Planta (MkP Planta _) = True 
esDeTipo Fuego (MkP Fuego _)   = True 
esDeTipo _ _                   = False

-- Dados un entrenador y un pokemon devuelve True si el entrenador
-- posee un pokemon que le gane a ese pokemon.
--
lePuedeGanar :: Entrenador -> Pokemon -> Bool
lePuedeGanar (MkE _ []) _       = False 
lePuedeGanar (MkE _ (p : ps)) o =
	leGanaA p p' || lePuedeGanar (MkE _ ps) o 


-- Indica si el entrenador tiene al menos un pokemon del tipo dado.
--
tieneAlMenosUnPokemonDeTipo :: TipoDePokemon -> Entrenador -> Bool
tieneAlMenosUnPokemonDeTipo t e = 
	cantidadDePokemonsDeTipo t (pokemonsE e) >= 1


-- Dados un tipo de pokemon y dos entrenadores, devuelve True
-- si ambos entrenadores tienen al menos un pokemon de ese tipo
-- y que tenga energia para pelear.
--
puedenPelear :: TipoDePokemon -> Entrenador -> Entrenador -> Bool
puedenPelear t e otro = 
	tieneUnPokemonDeTipoConEnergia t e && 
	tieneUnPokemonDeTipoConEnergia t otro

tieneUnPokemonDeTipoConEnergia :: TipoDePokemon -> Entrenador -> Bool
tieneUnPokemonDeTipoConEnergia t e =
	existeUnPokemonDeTipoConEnergia t (pokemonsE e)

existeUnPokemonDeTipoConEnergia :: TipoDePokemon -> [Pokemon] -> Bool
existeUnPokemonDeTipoConEnergia _ []       = False 
existeUnPokemonDeTipoConEnergia t (p : ps) = 
	(esDeTipo t p && energiaP p > 0) 
	|| existeUnPokemonDeTipoConEnergia t ps

-- Dado un entrenador devuelve True si ese entrenador posee
-- al menos un pokemon de cada tipo posible.
--
esExperto :: Entrenador -> Bool
esExperto e = 
	tieneAlMenosUnPokemonDeTipo Agua e   && 
	tieneAlMenosUnPokemonDeTipo Planta e &&
	tieneAlMenosUnPokemonDeTipo Fuego e


-- Dada una lista de entrenadores devuelve una lista con todos
-- los pokemons de cada entrenador.
--
fiestaPokemon :: [Entrenadores] -> [Pokemon]
fiestaPokemon []       = []
fiestaPokemon (e : es) = pokemonsE e ++ fiestaPokemon es
