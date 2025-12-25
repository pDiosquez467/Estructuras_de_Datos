
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
gramosDeHarinaTotales :: Int -> Int -> Int
gramosDeHarinaTotales invitados porcionesPorInvitado =
	 pizzasTotales invitados porcionesPorInvitado *
	 gramosPorPizza
