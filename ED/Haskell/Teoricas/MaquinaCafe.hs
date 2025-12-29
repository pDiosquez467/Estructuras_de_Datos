module MaquinaCafe
  ( -- * Tipos exportados
    TipoCafe(..)
  , MaquinaCafe    -- Se exporta el tipo, pero NO el constructor (TDA Opaco)
    -- * Constructores y Comandos
  , nuevaMC        -- :: MaquinaCafe
  , pedirCafeMC    -- :: MaquinaCafe -> TipoCafe -> MaquinaCafe
  , mantenerMC     -- :: MaquinaCafe -> MaquinaCafe
    -- * Consultas
  , disponibleMC   -- :: MaquinaCafe -> TipoCafe -> Bool
  , recaudacionMC  -- :: MaquinaCafe -> Int
  ) where

-------------------------------------------------------------------------------
-- INTERFAZ
-------------------------------------------------------------------------------

data TipoCafe = CafeSolo
              | CafeDulce
              | CafeCortado
              deriving Show

-- | El tipo abstracto de dato. Oculta su estructura interna (agua, leche, etc).
data MaquinaCafe = MC Int -- cantidad de agua   (en cc)
                      Int -- cantidad de leche  (en cc)
                      Int -- cantidad de café   (en g)
                      Int -- cantidad de azúcar (en g)
                      Int -- recaudación        ($)
                      deriving Show

-------------------------------------------------------------------------------
-- IMPLEMENTACIÓN DE LA INTERFAZ
-------------------------------------------------------------------------------

-- | Crea una máquina vacía y sin recaudación.
nuevaMC :: MaquinaCafe
nuevaMC = MC 0 0 0 0 0

-- | Verifica si hay recursos suficientes para el tipo de café solicitado.
disponibleMC :: MaquinaCafe -> TipoCafe -> Bool
disponibleMC (MC agua leche cafe azucar _) tc =
  agua      >= aguaRequerida tc  &&
  leche     >= lecheRequerida tc &&
  cafe      >= cafeRequerido tc  &&
  azucar    >= azucarRequerida tc

-- | Sirve un café, descontando recursos y aumentando la recaudación.
-- PRE: Debe haberse verificado previamente con 'disponibleMC'.
pedirCafeMC :: MaquinaCafe -> TipoCafe -> MaquinaCafe
pedirCafeMC (MC agua leche cafe azucar recaudacion) tc =
  MC (agua   - aguaRequerida tc)
     (leche  - lecheRequerida tc)
     (cafe   - cafeRequerido tc)
     (azucar - azucarRequerida tc)
     (recaudacion + precioCafe tc)

-- | Recarga la máquina a su capacidad máxima y reinicia la recaudación a 0.
mantenerMC :: MaquinaCafe -> MaquinaCafe
mantenerMC _ =
  MC capacidadMaximaAgua
     capacidadMaximaLeche
     capacidadMaximaCafe
     capacidadMaximaAzucar
     0

-- | Devuelve el monto total recaudado hasta el momento.
recaudacionMC :: MaquinaCafe -> Int
recaudacionMC (MC _ _ _ _ recaudacion) = recaudacion

-------------------------------------------------------------------------------
-- FUNCIONES AUXILIARES Y CONSTANTES
-------------------------------------------------------------------------------

-- Costos de recursos por tipo de café
aguaRequerida :: TipoCafe -> Int
aguaRequerida CafeSolo    = 200
aguaRequerida CafeDulce   = 200
aguaRequerida CafeCortado = 150

lecheRequerida :: TipoCafe -> Int
lecheRequerida CafeSolo    = 0
lecheRequerida CafeDulce   = 0
lecheRequerida CafeCortado = 50

cafeRequerido :: TipoCafe -> Int
cafeRequerido CafeSolo    = 40
cafeRequerido CafeDulce   = 40
cafeRequerido CafeCortado = 30

azucarRequerida :: TipoCafe -> Int
azucarRequerida CafeSolo    = 0
azucarRequerida CafeDulce   = 12
azucarRequerida CafeCortado = 12

precioCafe :: TipoCafe -> Int
precioCafe _ = 100

-- Capacidades máximas de la máquina
capacidadMaximaAgua, capacidadMaximaLeche, capacidadMaximaCafe, capacidadMaximaAzucar :: Int
capacidadMaximaAgua   = 20000
capacidadMaximaLeche  = 2000
capacidadMaximaCafe   = 1000
capacidadMaximaAzucar = 1000