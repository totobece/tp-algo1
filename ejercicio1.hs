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
sinVuelosDuplicados (v:vs) = not (vueloDuplicado v vs) && sinVuelosDuplicados vs
                        where 
                            vueloDuplicado :: Vuelo -> [Vuelo] -> Bool
                            vueloDuplicado _ [] = False
                            vueloDuplicado v1 (v2:vs) | verificarMismoVuelo v1 v2 = True
                                                      | otherwise = vueloDuplicado v1 vs
                            

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vs) = vueloValido v && sinVuelosDuplicados (v:vs) && vuelosValidos vs 