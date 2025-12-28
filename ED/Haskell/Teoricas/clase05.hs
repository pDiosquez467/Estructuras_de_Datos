
-- ESPÍAS
--

type Cod = Int

type Ciudad = String

data Agencia = Agente Cod Ciudad
             | Jefe Cod Agencia Agencia
             deriving Show

codA :: Agencia -> Cod
codA (Agente c _) = c
codA (Jefe c _ _) = c

-- Pre: La agencia debe ser de un solo agente. 
--
ciudadA :: Agencia -> Ciudad
ciudadA (Agente _ c) = c

--
nroEspias :: Agencia -> Int
nroEspias (Agente _ _ ) = 1
nroEspias (Jefe _ agencia1 agencia2) =
    1 + nroEspias agencia1 + nroEspias agencia2

-- Propósito:
-- Indica si el espía identificado por el código dado pertenece 
-- a la agencia. 
--
esEspiaDe :: Agencia -> Cod -> Bool
esEspiaDe (Agente cod _) cod'               = cod == cod'
esEspiaDe (Jefe cod agencia1 agencia2) cod' =
    cod == cod' || esEspiaDe agencia1 cod' || esEspiaDe agencia2 cod'

-- 
agentesRadicadosEn :: Agencia -> Ciudad -> [Cod]
agentesRadicadosEn (Agente cod ciudad) ciudad'          = [cod | ciudad == ciudad']
agentesRadicadosEn (Jefe cod agencia1 agencia2) ciudad' =
    agentesRadicadosEn agencia1 ciudad' ++
    agentesRadicadosEn agencia2 ciudad'

-- Propósito:
-- Dada una agencia, devuelve la agencia que resultara de 
-- cambiar el código viejo de un espía por el código nuevo.
--
renombrarEspia :: Agencia -> Cod -> Cod -> Agencia
renombrarEspia (Agente cod ciudad) cv cn          = Agente (renombrarCodigo cod cv cn) ciudad
renombrarEspia (Jefe cod agencia1 agencia2) cv cn =
    Jefe (renombrarCodigo cod cv cn) (renombrarEspia agencia1 cv cn) (renombrarEspia agencia2 cv cn)

--
renombrarCodigo :: Cod -> Cod -> Cod -> Cod
renombrarCodigo cod cv cn | cod == cv = cn
                          | otherwise = cv

-- Propósito:
-- Dada una agencia y un código de espía, devuelve
-- la subagencia comandada por ese espía. 
-- Pre: 
-- El código de espía debe pertenecer a la agencia. 
--
subagencia :: Agencia -> Cod -> Agencia
subagencia (Agente cod ciudad) _ = Agente cod ciudad
subagencia (Jefe cod agencia1 agencia2) cod'
  | cod == cod'             = Jefe cod agencia1 agencia2
  | esEspiaDe agencia1 cod' = subagencia agencia1 cod'
  | otherwise               = subagencia agencia2 cod'

-- Propósito:
-- Dada una agencia y un código de espía, devuelve una lista
-- que incluye a todos (y solamente) a los subordinados de 
-- dicho espía. 
-- Pre:
-- El código de espía debe pertenecer a la agencia.
-- Obs:
-- Todo espía es subordinados de sí mismo. 
--
subordinados :: Agencia -> Cod -> [Cod]
subordinados agencia cod =
    todosLosEspias (subagencia agencia cod)

--
todosLosEspias :: Agencia -> [Cod]
todosLosEspias (Agente cod _ )              = [cod]
todosLosEspias (Jefe cod agencia1 agencia2) =
    [cod] ++ todosLosEspias agencia1 ++ todosLosEspias agencia2

-- Propósito:
-- Dada una agencia y un código de espía, devuelve la lista 
-- de espías por los que hay que pasar para llegar desde la
-- raíz de la agencia hasta el espía indicado. 
-- Pre:
-- El código de espía debe pertenecer a la agencia. 
-- 
caminoHasta :: Agencia -> Cod -> [Cod]
caminoHasta (Agente cod _ ) _ = [cod]
caminoHasta (Jefe cod agencia1 agencia2) cod'
  | cod == cod'             = [cod]
  | esEspiaDe agencia1 cod' = cod : caminoHasta agencia1 cod'
  | otherwise               = cod : caminoHasta agencia2 cod'