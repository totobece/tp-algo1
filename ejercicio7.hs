type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]


vueloValido :: Vuelo -> Bool
vueloValido (ciudadOrigen, ciudadDestino, duracionViaje) = duracionViaje > 0 && ciudadOrigen /= ciudadDestino

verificarMismoVuelo :: Vuelo -> Vuelo -> Bool
verificarMismoVuelo (ciudadOrigen1, ciudadDestino1, _) (ciudadOrigen2, ciudadDestino2, _) = ciudadOrigen1 == ciudadOrigen2 && ciudadDestino1 == ciudadDestino2


sinVuelosDuplicados :: [Vuelo] -> Bool
sinVuelosDuplicados [] = True
sinVuelosDuplicados (v:vs) = not (any(verificarMismoVuelo v) vs) && sinVuelosDuplicados vs

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vs) = vueloValido v && sinVuelosDuplicados (v:vs) && vuelosValidos vs 


puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen [] _ = False
puedoVolverAOrigen agencia origen | not (vuelosValidos agencia) = False 
                                  | otherwise =  existeRuta origen origen agencia

existeRuta :: Ciudad -> Ciudad -> AgenciaDeViajes -> Bool
existeRuta origen1 destino1 [] = False
existeRuta origen1 destino1 ((origen2, destino2, _):vuelos)
    | origen1 == destino1 = True
    | origen2 == origen1  = existeRuta destino2 destino1 vuelos
    | otherwise = existeRuta origen1 destino1 vuelos


{-- 
[("Cordoba", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0),("Cordoba", "BsAs", 8.0)] Cordoba = True
[("Cordoba", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0),("Cordoba", "BsAs", 8.0)] Ciudad  =   --}
