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


--Esta funcion toma datos de tipo AgenciaDeViajes y llama a la lista ciudadMayorApariciones enviandole como parametro (listaConcatenadaCiudadApariciones vuelos) que es una lista tipo [("Rosario",2)]
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [(x,y,z)] = x  
ciudadMasConectada vuelos = ciudadMayorApariciones (listaConcatenadaCiudadApariciones vuelos)

--Esta funcion toma una lista tipo [("Rosario",2),("BsAs", 3)] y recursiona para que ciudad tiene mas apariciones (ciudad,apariciones)
ciudadMayorApariciones :: [(Ciudad,Int)] -> Ciudad
ciudadMayorApariciones [(ciudad,apariciones)] = ciudad
ciudadMayorApariciones ((ciudadA,aparicionesA):(ciudadB,aparicionesB):resto) 
    | aparicionesA > aparicionesB = ciudadMayorApariciones ((ciudadA,aparicionesA):resto)
    | otherwise = ciudadMayorApariciones ((ciudadB,aparicionesB):resto)

--Aca llamamos a concatenarCiudadApariciones enviandole una lista ordenada de ciudades ["Rosario","Jujuy"]
listaConcatenadaCiudadApariciones :: AgenciaDeViajes -> [(Ciudad,Int)]
listaConcatenadaCiudadApariciones [] = []
listaConcatenadaCiudadApariciones vuelos = concatenarCiudadApariciones (transformarLista vuelos) 
    where   
        --Esta funcion toma datos de tipo AgenciaDeViajes, la cual transforma en una lista de ciudades [("Rosario","Jujuy",5.0)] => ["Rosario","Jujuy"]
        transformarLista :: AgenciaDeViajes -> [Ciudad]
        transformarLista [] = []
        transformarLista ((ciudad1,ciudad2,_):vuelos) = ciudad1 :ciudad2 :transformarLista vuelos

--Toma una lista de ciudades y crea una tupla con el primer elemento de la lista y con la cantidad de veces que aparece [("Rosario",2),("BsAs", 3)] y concatena la recursion.
concatenarCiudadApariciones :: [Ciudad] -> [(Ciudad,Int)]
concatenarCiudadApariciones [] = []
concatenarCiudadApariciones (ciudad:ciudades) = (ciudad,contarApariciones ciudad (ciudad:ciudades) ) : concatenarCiudadApariciones ciudades 

--Toma una ciudad y recursiona en toda la lista para devolver la cantidad de veces que aparece.
contarApariciones :: Ciudad -> [Ciudad] -> Int
contarApariciones _ []  = 0
contarApariciones lugar (ciudad:ciudades)
    |  lugar == ciudad = 1 + contarApariciones lugar ciudades 
    |  otherwise = contarApariciones lugar ciudades 


-- EJERCICIO 5


--Hay dos formas de llegar, directa o con escala. Si una de las dos es verdadera entonces puedo llegar. 
--En sePuedeLLegarEscala enviamos dos listas para trabajar con una y recursionar con la otra.
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False 
sePuedeLlegar vuelos origen destino = (sePuedeLlegarDirecto vuelos origen destino) || (sePuedeLlegarEscala vuelos vuelos origen destino)

--Toma la lista de AgenciaDeViajes y verifica que haya un vuelo directo con el primer elementod de la lista sino es asi, recursiona y si al menos uno es True => True.
sePuedeLlegarDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarDirecto [] _ _ = False
sePuedeLlegarDirecto (vuelo:vuelos) origen destino = (comparaCiudades vuelo origen destino) || (sePuedeLlegarDirecto vuelos origen destino)
    where
        comparaCiudades :: Vuelo -> Ciudad -> Ciudad -> Bool
        comparaCiudades (partida,llegada,_) origen destino = (partida == origen) && (llegada == destino)

--Esta llama a la funcion compararVuelosEscala, en la cual verifica si se puede llegar en escala sino recursiona en la funcion.
sePuedeLlegarEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarEscala _ [] _ _ = False
sePuedeLlegarEscala vuelosCompletos ((partida,llegada,_):vuelos) origen destino
    | compararVuelosEscala partida llegada vuelosCompletos origen destino = True
    | otherwise = sePuedeLlegarEscala vuelosCompletos vuelos origen destino

--Aca es donde nos importa haber tenido dos listas, ya que tenemos una completa para recursionar en ella y las otras ciudades por separado las cuales vamos a comparar.
--Para saber si el vuelo se realiza en escala.
compararVuelosEscala :: Ciudad -> Ciudad -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
compararVuelosEscala  _ _ [] _ _ = False
compararVuelosEscala  ciudadPartida ciudadLlegada ((partida,llegada,_):vuelos) origen destino
    |(ciudadPartida == origen) && (ciudadLlegada == partida) = compararLlegadaDestino llegada destino
    |otherwise = compararVuelosEscala ciudadPartida ciudadLlegada vuelos origen destino
    where
        compararLlegadaDestino :: Ciudad -> Ciudad -> Bool
        compararLlegadaDestino llegada destino = llegada == destino


-- EJERCICIO 6


-- Calcula la duración del camino más rápido entre dos ciudades en una agencia de viajes.
-- Considera tanto los vuelos directos como los que incluyen escalas.
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido vuelos origen destino = calcularMinimoLista (listasDuracionVuelosEscala vuelos vuelos origen destino ++ listaDuracionVuelosDirectos vuelos origen destino)
    where
        calcularMinimoLista :: [Duracion] -> Duracion
        calcularMinimoLista [x] = x
        calcularMinimoLista (x:xs)
            | x < calcularMinimoLista xs = x 
            | otherwise = calcularMinimoLista xs 

-- Genera una lista de las duraciones de los vuelos directos entre dos ciudades.
listaDuracionVuelosDirectos :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
listaDuracionVuelosDirectos [] _ _ = [] 
listaDuracionVuelosDirectos ((partida,llegada,duracion):vuelos) origen destino
    | (partida == origen) && (llegada == destino) = [duracion]
    | otherwise = listaDuracionVuelosDirectos vuelos origen destino

-- Genera una lista de las duraciones de los vuelos con escalas entre dos ciudades.
listasDuracionVuelosEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
listasDuracionVuelosEscala _ [] _ _ = [] 
listasDuracionVuelosEscala vuelosCompletos (vuelo:vuelos) origen destino = calcularDuracionVuelosEscala vuelo vuelosCompletos origen destino ++ listasDuracionVuelosEscala vuelosCompletos vuelos origen destino

-- Calcula la duración de un vuelo con escala, si es posible.
calcularDuracionVuelosEscala :: (Ciudad,Ciudad,Duracion) -> AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
calcularDuracionVuelosEscala _ [] _ _ = []
calcularDuracionVuelosEscala (partidaA,llegadaA,duracionA) ((partidaB,llegadaB,duracionB):vuelos) origen destino
    | (partidaA == origen) && (llegadaA == partidaB) && (llegadaB == destino) = [duracionB + duracionA] 
    | otherwise = calcularDuracionVuelosEscala (partidaA,llegadaA,duracionA) vuelos origen destino


-- EJERCICIO 7


--Usamos el mismo criterio que en el ejercicio 5, evaluamos si podemos volver al origen directamente o debemos volver en una escala.
--Enviamos dos listas a cada funcion para que sea mas comodo trabajar. 
puedeVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedeVolverAOrigen [] _ = False
puedeVolverAOrigen vuelos origen = verificarVueloDirecto vuelos vuelos origen || verificarVueloConEscala vuelos vuelos origen

-- Verifica si hay un vuelo directo que regrese a la ciudad de origen
verificarVueloDirecto :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Bool
verificarVueloDirecto _ [] _ = False
verificarVueloDirecto listaVuelosTotales ((partida, llegada, _):vuelos) origen
    | origen == partida = esVueloDirecto listaVuelosTotales (partida, llegada, 0) origen || verificarVueloDirecto listaVuelosTotales vuelos origen
    | otherwise = verificarVueloDirecto listaVuelosTotales vuelos origen

--Verifica si es posible regresar a la ciudad de origen haciendo una escala
verificarVueloConEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Bool
verificarVueloConEscala _ [] _ = False
verificarVueloConEscala listaVuelosTotales ((partida, llegada, _):vuelos) origen
    | origen == partida = esVueloConEscala listaVuelosTotales (partida, llegada, 0) listaVuelosTotales origen || verificarVueloConEscala listaVuelosTotales vuelos origen
    | otherwise = verificarVueloConEscala listaVuelosTotales vuelos origen

--Verifica si hay un vuelo directo de regreso al origen
esVueloDirecto :: AgenciaDeViajes -> Vuelo -> Ciudad -> Bool
esVueloDirecto [] _ _ = False  
esVueloDirecto ((partidaA, llegadaA, _):vuelos) (partida, llegada, _) origen 
    | llegada == partidaA = llegadaA == origen || esVueloDirecto vuelos (partida, llegada, 0) origen
    | otherwise = esVueloDirecto vuelos (partida, llegada, 0) origen

--Verifica si hay un vuelo con escala que regrese a la ciudad de origen
esVueloConEscala :: AgenciaDeViajes -> Vuelo -> AgenciaDeViajes -> Ciudad -> Bool
esVueloConEscala [] _ _ _ = False  
esVueloConEscala ((partidaA, llegadaA, _):vuelos) (partida, llegada, _) listaVuelosTotales origen 
    | llegada == partidaA = verificarEscala (partidaA, llegadaA, 0) listaVuelosTotales origen || esVueloConEscala vuelos (partida, llegada, 0) listaVuelosTotales origen
    | otherwise = esVueloConEscala vuelos (partida, llegada, 0) listaVuelosTotales origen

--Verifica si es posible hacer una escala y regresar al origen
verificarEscala :: Vuelo -> AgenciaDeViajes -> Ciudad -> Bool
verificarEscala _ [] _ = False
verificarEscala (partida, llegada, _) ((partidaA, llegadaA, _):vuelos) origen
    | llegada == partidaA = llegadaA == origen || verificarEscala (partida, llegada, 0) vuelos origen
    | otherwise = verificarEscala (partida, llegada, 0) vuelos origen