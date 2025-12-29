import MaquinaCafe

-- Propósito: 
-- Devuelve la cantidad de cortados que se podrían
-- consumir usando esta máquina de café. 
--
cuantosCortados :: MaquinaCafe -> Int
cuantosCortados m =
    if disponibleMC m CafeCortado
        then 1 + cuantosCortados (pedirCafeMC m CafeCortado)
        else 0

-- Propósito:
-- Devuelve el total recaudado en todas las máquinas de café del
-- local. 
--
totalRecaudacion :: [MaquinaCafe] -> Int
totalRecaudacion = foldr ((+) . recaudacionMC) 0 