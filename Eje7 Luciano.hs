module TestSolucion where


type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

-- FUNCION DEL EJERCICIO 1 PARA EJECUTAR EL 5 Y 6

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vs) = vueloValido v && sinVuelosDuplicados (v:vs) && vuelosValidos vs 

sinVuelosDuplicados :: [Vuelo] -> Bool
sinVuelosDuplicados [] = True
sinVuelosDuplicados (v:vs) = not (any(verificarMismoVuelo v) vs) && sinVuelosDuplicados vs

vueloValido :: Vuelo -> Bool
vueloValido (ciudadOrigen, ciudadDestino, duracionViaje) = duracionViaje > 0 && ciudadOrigen /= ciudadDestino

verificarMismoVuelo :: Vuelo -> Vuelo -> Bool
verificarMismoVuelo (ciudadOrigen1, ciudadDestino1, _) (ciudadOrigen2, ciudadDestino2, _) = ciudadOrigen1 == ciudadOrigen2 && ciudadDestino1 == ciudadDestino2


-- EJERCICIO 7

puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen 
    |vuelosValidos vuelos = puedoVolverAOrigenDirecto vuelos vuelos origen || puedoVolverAOrigenEscala vuelos vuelos origen
    |otherwise = False

puedoVolverAOrigenDirecto :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigenDirecto _ [] _ = False
puedoVolverAOrigenDirecto listaVuelosTotales ((partida,llegada,duracion):vuelos) origen 
    | origen == partida = compararLlegadaOrigenDirecto listaVuelosTotales (partida,llegada,duracion) origen || puedoVolverAOrigenDirecto listaVuelosTotales vuelos origen
    | otherwise = puedoVolverAOrigenDirecto listaVuelosTotales vuelos origen

compararLlegadaOrigenDirecto :: AgenciaDeViajes -> Vuelo -> Ciudad -> Bool
compararLlegadaOrigenDirecto [] _ _ = False  
compararLlegadaOrigenDirecto ((partidaA,llegadaA,duracionA):vuelos) (partida,llegada,duracion) origen 
    | llegada == partidaA = llegadaA == origen || compararLlegadaOrigenDirecto vuelos (partida,llegada,duracion) origen
    | otherwise = compararLlegadaOrigenDirecto vuelos (partida,llegada,duracion) origen

puedoVolverAOrigenEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigenEscala _ [] _ = False
puedoVolverAOrigenEscala listaVuelosTotales ((partida,llegada,duracion):vuelos) origen 
    | origen == partida = compararLlegadaOrigenEscala listaVuelosTotales (partida,llegada,duracion) listaVuelosTotales origen || puedoVolverAOrigenEscala listaVuelosTotales vuelos origen
    | otherwise = puedoVolverAOrigenEscala listaVuelosTotales vuelos origen 


compararLlegadaOrigenEscala :: AgenciaDeViajes -> Vuelo ->  AgenciaDeViajes -> Ciudad  -> Bool
compararLlegadaOrigenEscala [] _ _ _ = False  
compararLlegadaOrigenEscala ((partidaA,llegadaA,duracionA):vuelos) (partida,llegada,duracion) listaVuelosTotales origen 
    | llegada == partidaA = compararLlegadaOrigenEscalaAux (partidaA,llegadaA,duracionA) listaVuelosTotales origen || compararLlegadaOrigenEscala vuelos (partida,llegada,duracion) listaVuelosTotales origen
    | otherwise = compararLlegadaOrigenEscala vuelos (partida,llegada,duracion) listaVuelosTotales origen


compararLlegadaOrigenEscalaAux:: Vuelo -> AgenciaDeViajes -> Ciudad  -> Bool
compararLlegadaOrigenEscalaAux _ [] _ = False
compararLlegadaOrigenEscalaAux (partida,llegada,duracion) ((partidaA,llegadaA,duracionA):vuelos) origen
    | llegada == partidaA = llegadaA == origen || compararLlegadaOrigenEscalaAux (partida,llegada,duracion) vuelos origen
    | otherwise = compararLlegadaOrigenEscalaAux (partida,llegada,duracion) vuelos origen

-- puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0),("Cordoba", "BsAs", 8.0)] "BsAs"
