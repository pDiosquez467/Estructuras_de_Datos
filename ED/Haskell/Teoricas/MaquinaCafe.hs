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
