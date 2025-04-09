module Library where
import PdePreludat

data TipoPokemon = Planta | Agua | Fuego deriving (Show, Eq)

tieneVentajaContra :: TipoPokemon -> TipoPokemon -> Bool
tieneVentajaContra Planta Agua = True
tieneVentajaContra Agua Fuego = True
tieneVentajaContra Fuego Planta = True
tieneVentajaContra _ _ = False

data Pokemon = UnPokemon { 
  nombre :: String, 
  tipo :: TipoPokemon
} deriving (Show, Eq)

--Punto 1

lesPuedeGanarA :: Pokemon -> [Pokemon] -> [Pokemon]
lesPuedeGanarA atacante contrincantes = filter (leGana atacante) contrincantes

leGana :: Pokemon -> Pokemon -> Bool
leGana pokeAtacante pokeDefensor = tieneVentajaContra (tipo pokeAtacante) (tipo pokeDefensor)

charmander = UnPokemon "Charmander" Fuego
flareon = UnPokemon "Flareon" Fuego
squirtle = UnPokemon "Squirtle" Agua
oddish = UnPokemon "Oddish" Planta
gyarados = UnPokemon "Gyarados" Agua
carpinchos = UnPokemon "Carpinchos" Agua

-- Punto 2

data Destino = UnGimnasio { nombreGym:: String, siguiente:: Destino }
              | UnaLiga { contrincantes:: [Pokemon] } deriving (Show, Eq)

gymRoca :: Destino
gymRoca = UnGimnasio "Gimnasio Roca" gymAgua
gymAgua :: Destino
gymAgua = UnGimnasio "Gimnasio Agua" gymElectrico
gymElectrico :: Destino
gymElectrico = UnGimnasio "Gimnasio Electrico" ligaKanto
ligaKanto :: Destino
ligaKanto = UnaLiga [flareon, gyarados, charmander]
gymFuego :: Destino
gymFuego = UnGimnasio "Gimnasio Fuego" gymPlanta
gymPlanta :: Destino
gymPlanta = UnGimnasio "Gimnasio Planta" ligaGali
ligaGali :: Destino
ligaGali = UnaLiga [carpinchos, gyarados]

estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno _ (UnGimnasio _ _) = True
estaAlHorno pokemon (UnaLiga contrincantes) = all (pierdeContra pokemon) contrincantes

pierdeContra :: Pokemon -> Pokemon -> Bool
pierdeContra pokemon contrincante = leGana contrincante pokemon

-- Punto 3 

puedeViajar:: Destino -> Destino -> Bool
puedeViajar (UnaLiga contrincantes) destino = (UnaLiga contrincantes) == destino
puedeViajar origen destino = origen == destino || puedeViajar (siguiente origen) destino

-- Punto 4

cantidadVictorias :: Pokemon -> [Pokemon] -> Number
cantidadVictorias atacante = length.lesPuedeGanarA atacante

-- Punto 5

elMasPicante :: [Pokemon] -> Pokemon
elMasPicante pokemones = foldl1 (elMejorDeDos pokemones) pokemones

elMejorDeDos :: [Pokemon] -> Pokemon -> Pokemon -> Pokemon
elMejorDeDos pokemones poke1 poke2
            | cantidadVictorias poke1 pokemones >= cantidadVictorias poke2 pokemones = poke1
            | otherwise = poke2

--aaaaa