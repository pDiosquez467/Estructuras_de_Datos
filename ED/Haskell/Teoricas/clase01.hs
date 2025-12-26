

-- EJEMPLO
{-- 
	Roque quiere cocinar pizza para n invitados, cado uno de 
	los cuales come m porciones de pizza. Una pizza tiene 8
	porciones. Cada pizza require 250g de harina. ¿Cuánta harina
	debería comprar?
--}

porcionesPorPizza :: Int 
porcionesPorPizza = 8

gramosDeHarinaPorPizza :: Int 
gramosDeHarinaPorPizza = 250

porcionesTotales :: Int -> Int -> Int 
porcionesTotales invitados porcionesPorInvitado = 
	invitados * porcionesPorInvitado 

pizzasTotales :: Int -> Int -> Int
pizzasTotales invitados porcionesPorInvitado = 
	div (porcionesTotales invitados porcionesPorInvitado)
		 porcionesPorPizza

-- Propósito: 
-- Calcula la cantidad de gramos de harina en total que hay
-- que comprar para preparar pizza para 'invitados' invitados
-- suponiendo que cada invitado come 'porcionesPorInvitado'.
--
gramosDeHarinaTotales :: Int -> Int -> Int
gramosDeHarinaTotales invitados porcionesPorInvitado =
	 pizzasTotales invitados porcionesPorInvitado *
	 gramosPorPizza

-- FUNCIONES

f :: Int -> Int
f x = x + 1

g :: Int -> Int -> Int
g x y = x * y + y

h :: Bool -> Int -> Int -> Bool
h b x y = b && x < y

-- Convenciones de asociatividad y precedencia:
{--
	Hay que poner paréntesis cuando el argumento a una función
	es el resultado de invocar a otra función:
		f (f (f 9))
		g (f 3) (f 4)

	Los operadores lógicos tienen menor precedencia que las 
	funciones.
--}

-- EJERCICIO
{--
	Roque quiere determinar cuántos segundos en total hay en
	d días, h horas, m minutos y s segundos. Definir una función:
	segundosEnTotal d h m s = ...
	para resolver este problema.
--}

horasEnTotal :: Int -> Int -> Int
horasEnTotal d h = 24 * d + h

minutosEnTotal :: Int -> Int -> Int -> Int 
minutosEnTotal d h m = horasEnTotal d h * 60 + m

-- Propósito:
-- Calcula el número de segundos que hay en total en 'd' días,
-- 'h' horas, 'm' minutos y 's' segundos.
-- 
segundosTotales :: Int -> Int -> Int -> Int -> Int 
segundosTotales d h m s = 
	minutosEnTotal d h m * 60 + s

-- ALTERNATIVA CONDICIONAL
-- El 'if' no es un comando, sino una expresión.
-- Al ser una expresión, tiene un valor y por tanto siempre
-- debe tener una rama 'then' y otra 'else'.

expr1 :: Int 
expr1 = (if False then 1 else 2) * 3

expr2 :: Char
expr2 = if False then 'a' else 'b'

-- Propósito:
-- Dados dos números denota el menor de ellos.
--
minimo :: Int -> Int -> Int
minimo x y = if x < y 
			  	then x 
				else y

-- EJERCICIO

minimo3 :: Int -> Int -> Int -> Int 
minimo3 x y z = minimo (minimo x y) z 

maximo3 :: Int -> Int -> Int -> Int 
maximo3 x y z = maximo (maximo x y) z
				where
					maximo :: Int -> Int -> Int
					maximo x y = if x > y 
									then x 
									else y 

medio3 :: Int -> Int -> Int -> Int
medio3 x y z = x + y + z - minimum [x,y,z] - maximum [x,y,z]

-- SINÓNIMOS DE TIPOS

-- type Edad = Int

-- edadDeRoque :: Edad
-- edadDeRoque = 32 

-- PARES
-- Propósito:
-- Dado un par, devuelve un nuevo par con las componentes del 
-- original invertidas.
--
dadoVuelta :: (a, b) -> (b, a)
dadoVuelta par = (snd par, fst par)

-- PATTERN MATCHING
--
dadoVueltaBis :: (a, b) -> (b, a)
dadoVueltaBis (x, y) = (y, x)


-- EJERCICIO
-- Definir una función sobre pares que tenga el siguiente tipo:
-- f :: (a, (b, c)) -> ((a, b), c)

f :: (a, (b, c)) -> ((a, b), c)
f (x, (y, z)) = ((x, y), z)

fBis :: (a, (b, c)) -> ((a, b), c)
fBis p = ((fst p, fst sp), snd sp)
		 where sp = snd p


-- TIPOS ENUMERATIVOS
--
data Simpson = Homero 
			 | Marge 
			 | Bart 
			 | Lisa 
			 | Maggie
       		 deriving Show

type Edad = Int 

edad :: Simpson -> Edad
edad Homero = 36
edad Marge  = 34
edad Bart   = 10
edad Lisa   = 8
edad Maggie = 1 

-- 'madre' es una función parcial
-- Propósito: Dado un Simpson, devuelve a su madre.
-- Pre: El Simpson debe ser 'Bart', 'Lisa' o 'Maggie'.
--
madre :: Simpson -> Simpson
madre Bart   = Marge
madre Lisa   = Marge
madre Maggie = Marge

-- EJERCICIO
--

type Cordenada = (Int, Int)

data Direccion = Norte
			   | Este
			   | Sur
			   | Oeste
			   deriving Show

-- Propósito: Devuelve la coordenada que resultaría de moverse
-- una unidad hacia la dirección indicada.
--
desplazar :: Coordenada -> Direccion -> Coordenada
desplazar (latitud, longitud) Norte = (latitud + 1, longitud) 
desplazar (latitud, longitud) Este  = (latitud, longitud + 1) 
desplazar (latitud, longitud) Sur   = (latitud - 1, longitud) 
desplazar (latitud, longitud) Oeste = (latitud, longitud - 1) 

