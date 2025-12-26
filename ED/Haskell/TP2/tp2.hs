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
