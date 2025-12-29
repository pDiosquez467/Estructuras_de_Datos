{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module MaquinaCafe where

---- Interfaz de la máquina de café

data TipoCafe = CafeSolo
              | CafeDulce
              | CafeCortado
              deriving Show

nuevaMC       :: MaquinaCafe
disponibleMC  :: MaquinaCafe -> TipoCafe -> Bool
-- Pre: El tipo de café debe estar disponible.
--
pedirCafeMC   :: MaquinaCafe -> TipoCafe -> MaquinaCafe
mantenerMC    :: MaquinaCafe -> MaquinaCafe
recaudacionMC :: MaquinaCafe -> Int

---- Implementación de la máquina de café 

data MaquinaCafe = MC Int -- cantidad de agua   (en cc)
                      Int -- cantidad de leche  (en cc)
                      Int -- cantidad de café   (en g)
                      Int -- cantidad de aźucar (en g)
                      Int -- recaudación        ($)

nuevaMC = MC 0 0 0 0 0

disponibleMC (MC agua leche cafe azucar _) tc = 
  agua      >= aguaRequerida tc 
  && leche  >= lecheRequerida tc 
  && cafe   >= cafeRequerido tc 
  && azucar >= azucarRequerida tc 

aguaRequerida :: TipoCafe -> Int 
aguaRequerida cafeSolo    = 200
aguaRequerida cafeDulce   = 200
aguaRequerida cafeCortado = 150

lecheRequerida :: TipoCafe -> Int 
lecheRequerida cafeSolo    = 0
lecheRequerida cafeDulce   = 0
lecheRequerida cafeCortado = 50

cafeRequerido :: TipoCafe -> Int  
cafeRequerido cafeSolo    = 40
cafeRequerido cafeDulce   = 40
cafeRequerido cafeCortado = 30

azucarRequerida :: TipoCafe -> Int 
azucarRequerida cafeSolo    = 0
azucarRequerida cafeDulce   = 12
azucarRequerida cafeCortado = 12

precioCafe :: TipoCafe -> Int 
precioCafe _ = 100 

capacidadMaximaAgua :: Int
capacidadMaximaAgua = 20000
capacidadMaximaLeche :: Int
capacidadMaximaLeche = 2000
capacidadMaximaCafe :: Int
capacidadMaximaCafe = 1000
capacidadMaximaAzucar :: Int
capacidadMaximaAzucar = 1000 
-- 
pedirCafeMC (MC agua leche cafe azucar recaudacion) tc = 
    MC 
    (agua   - aguaRequerida tc)
    (leche  - lecheRequerida tc)
    (cafe   - cafeRequerido tc)
    (azucar - azucarRequerida tc)
    (recaudacion + precioCafe tc)

--
mantenerMC _ = 
    MC 
    capacidadMaximaAgua
    capacidadMaximaLeche
    capacidadMaximaCafe 
    capacidadMaximaAzucar
    0


-- 
recaudacionMC (MC _ _ _ _ recaudacion) = recaudacion 