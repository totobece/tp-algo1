module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]


-- EJERCICIO 1


vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vs) = vueloValido v && sinVuelosDuplicados (v:vs) && vuelosValidos vs 

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


-- EJERCICIO 2


ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas agencia ciudad = eliminarDuplicados (vuelosHacia agencia ciudad ++ vuelosDesde agencia ciudad)

-- Función que encuentra las ciudades desde las cuales se puede volar a una ciudad dada
vuelosHacia :: AgenciaDeViajes -> Ciudad -> [Ciudad]
vuelosHacia  [] _ = []
vuelosHacia ((origen,destino,_): vuelos) ciudad | destino == ciudad = origen : vuelosHacia vuelos ciudad -- Si la ciudad de origen coincide, agregamos la ciudad
                                                | otherwise = vuelosHacia vuelos ciudad --Si no, sigo recorriendo la lista
-- Función que encuentra las ciudades a las que se puede volar desde una ciudad dada 
vuelosDesde :: AgenciaDeViajes -> Ciudad -> [Ciudad]
vuelosDesde [] _ = []
vuelosDesde ((origen,destino,_): vuelos) ciudad | origen == ciudad = destino : vuelosDesde vuelos ciudad
                                                | otherwise = vuelosDesde vuelos ciudad
--Funcion para eliminar duplicados
eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) | perteneceLista x xs = eliminarDuplicados xs
                          | otherwise = x : eliminarDuplicados xs
                        where 
                            perteneceLista x [] = False
                            perteneceLista x (y:ys) = (x == y) || perteneceLista x ys


-- EJERCICIO 3

modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota agencia | vuelosValidos agencia = modernizarVuelos agencia
                        | otherwise = []

modernizarVuelos :: AgenciaDeViajes -> AgenciaDeViajes
modernizarVuelos [] = []
modernizarVuelos ((origen, destino, duracion):vuelos) = (origen, destino, duracion * 0.9) : modernizarFlota vuelos


-- EJERCICIO 4


ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [(x,y,z)] = x  
ciudadMasConectada vuelos = saberMayor(listaContar vuelos)

listaCiudades :: AgenciaDeViajes -> [Ciudad]
listaCiudades [] = []
listaCiudades ((ciudad1,ciudad2,_):vuelos) = ciudad1 :ciudad2 :listaCiudades vuelos

listaContar :: AgenciaDeViajes -> [(Ciudad,Int)]
listaContar [] = []
listaContar vuelos = listaContarAux (listaCiudades vuelos) 

listaContarAux :: [Ciudad] -> [(Ciudad,Int)]
listaContarAux [] = []
listaContarAux (x:xs) = (x,contarApariciones x (x:xs) ) : listaContarAux xs 

contarApariciones :: Ciudad -> [Ciudad] -> Int
contarApariciones _ []  = 0
contarApariciones provincia (x:xs)
    |  provincia == x = 1 + contarApariciones provincia xs 
    |  otherwise = contarApariciones provincia xs 

saberMayor :: [(Ciudad,Int)] -> Ciudad
saberMayor [(x,y)] = x
saberMayor ((provincia,aparicion):(provinciay,apariciony):resto) 
    | aparicion > apariciony = saberMayor ((provincia,aparicion):resto)
    | otherwise = saberMayor ((provinciay,apariciony):resto)



-- EJERCICIO 5


sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False 
sePuedeLlegar vuelos origen destino = (sePuedeLlegarDirecto vuelos origen destino) || (sePuedeLlegarEscala vuelos vuelos origen destino)


sePuedeLlegarDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarDirecto [] _ _ = False
sePuedeLlegarDirecto (vuelo:vuelos) origen destino = (comparaPuntos vuelo origen destino) || (sePuedeLlegarDirecto vuelos origen destino)
    where
        comparaPuntos :: Vuelo -> Ciudad -> Ciudad -> Bool
        comparaPuntos (partida,llegada,_) origen destino = (partida == origen) && (llegada == destino)

sePuedeLlegarEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarEscala _ [] _ _ = False
sePuedeLlegarEscala vuelosCompletos ((partida,llegada,_):vuelos) origen destino
    | compararVuelosEscala partida llegada vuelosCompletos origen destino = True
    | otherwise = sePuedeLlegarEscala vuelosCompletos vuelos origen destino

compararVuelosEscala :: Ciudad -> Ciudad -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
compararVuelosEscala  _ _ [] _ _ = False
compararVuelosEscala  a b ((partida,llegada,_):vuelos) origen destino
    |(a == origen) && (b == partida) = compararLlegadaDestino llegada destino
    |otherwise = compararVuelosEscala a b vuelos origen destino
    where
        compararLlegadaDestino :: Ciudad -> Ciudad -> Bool
        compararLlegadaDestino llegada destino = llegada == destino


-- EJERCICIO 6


duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido vuelos origen destino = calcularMinimoLista (listasDuracionVuelosEscala vuelos vuelos origen destino ++ listaDuracionVuelosDirectos vuelos origen destino)
    where
        calcularMinimoLista :: [Duracion] -> Duracion
        calcularMinimoLista [x] = x
        calcularMinimoLista (x:xs)
            | x < calcularMinimoLista xs = x
            | otherwise = calcularMinimoLista xs

listaDuracionVuelosDirectos :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
listaDuracionVuelosDirectos [] _ _ = []
listaDuracionVuelosDirectos ((partida,llegada,duracion):vuelos) origen destino
    | (partida == origen) && (llegada == destino) = [duracion]
    | otherwise = listaDuracionVuelosDirectos vuelos origen destino

listasDuracionVuelosEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
listasDuracionVuelosEscala _ [] _ _ = []
listasDuracionVuelosEscala vuelosCompletos (vuelo:vuelos) origen destino = calcularDuracionVuelosEscala vuelo vuelosCompletos origen destino ++ listasDuracionVuelosEscala vuelosCompletos vuelos origen destino

calcularDuracionVuelosEscala :: (Ciudad,Ciudad,Duracion) -> AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
calcularDuracionVuelosEscala  _ [] _ _ = []
calcularDuracionVuelosEscala  (partidaA,llegadaA,duracionA) ((partidaB,llegadaB,duracionB):vuelos) origen destino
    |(partidaA == origen) && (llegadaA == partidaB) && (llegadaB == destino) = [duracionB + duracionA]
    |otherwise = calcularDuracionVuelosEscala (partidaA,llegadaA,duracionA) vuelos origen destino


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