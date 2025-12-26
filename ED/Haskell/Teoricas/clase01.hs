
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
