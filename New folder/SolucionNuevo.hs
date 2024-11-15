--Nombre grupo: somosdel_oeste8
-- Integrantes: Tobias Becerra, DNI 45076860 Email tobias.bec1704@gmail.com
--              Valentina Bolaños 44049734 Email valentinab19302@gmail.com 
--              Luciano Gonzalez 47097965 Email lucianogonzalez160122@gmail.com
--              (El alumno Maximiliano Alonso abandono la materia y fue reemplazado por Luciano Gonzalez)

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
modernizarFlota agencia = modernizarVuelos agencia

modernizarVuelos :: AgenciaDeViajes -> AgenciaDeViajes
modernizarVuelos [] = []
modernizarVuelos ((origen, destino, duracion):vuelos) = (origen, destino, duracion * 0.9) : modernizarFlota vuelos


-- EJERCICIO 4


--Aca llamamos a ciudadMayorApariciones enviandole una lista de vuelos [("A","B",4.0)] con otra lista que recursiona en la misma formando ["A","B"]
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [] = []  
ciudadMasConectada vuelos = ciudadMayorApariciones vuelos (transformarLista vuelos)

--Transforma una lista de vuelos [("A","C",4.0),("C","D",4.0)] ["A","B","C","D"]
transformarLista :: AgenciaDeViajes -> [Ciudad]
transformarLista [] = []
transformarLista ((ciudad1,ciudad2,_):vuelos) = ciudad1 :ciudad2 :transformarLista vuelos

--Calcula la longitud de una lista pasando una ciudad por ciudadesConectadas en vuelosCompletos
--[("A","C",4.0),("C","D",4.0)] "C" ["A","D"]
--Entonces la ciudad que pasa por ciudadesConectada con mayor longitud de lista es la ciudad que mas veces aparece.
ciudadMayorApariciones :: AgenciaDeViajes -> [Ciudad] -> Ciudad
ciudadMayorApariciones _ [ciudad] = ciudad
ciudadMayorApariciones vuelosCompletos (ciudad1:ciudad2:resto)
    | length(ciudadesConectadas vuelosCompletos ciudad1) >= length(ciudadesConectadas vuelosCompletos ciudad2) = ciudadMayorApariciones vuelosCompletos (ciudad1:resto)
    | otherwise = ciudadMayorApariciones vuelosCompletos (ciudad2:resto)
    

-- EJERCICIO 5


vuelosSinDuracion :: AgenciaDeViajes -> [(Ciudad,Ciudad)]
vuelosSinDuracion [] = []
vuelosSinDuracion ((partida,llegada,duracion):vuelos) = (partida,llegada) : vuelosSinDuracion vuelos

--Hay dos formas de llegar, directa o con escala. Si una de las dos es verdadera entonces puedo llegar. 
--En sePuedeLLegarEscala enviamos dos listas para trabajar con una y recursionar con la otra.
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False 
sePuedeLlegar vuelos origen destino = (sePuedeLlegarDirecto vuelos origen destino) || (sePuedeLlegarEscala (vuelosSinDuracion vuelos) vuelos origen destino)

--Toma la lista de AgenciaDeViajes y verifica que haya un vuelo directo con el primer elementod de la lista sino es asi, recursiona y si al menos uno es True => True.
sePuedeLlegarDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarDirecto [] _ _ = False
sePuedeLlegarDirecto ((partida,llegada,_):vuelos) origen destino = (partida == origen && llegada == destino) || (sePuedeLlegarDirecto vuelos origen destino)

--Esta llama a la funcion compararVuelosEscala, en la cual verifica si se puede llegar en escala sino recursiona en la funcion.
sePuedeLlegarEscala :: [(Ciudad,Ciudad)] -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarEscala _ [] _ _ = False
sePuedeLlegarEscala vuelosCompletos ((partida,llegada,_):vuelos) origen destino = (partida == origen && elem (llegada,destino) vuelosCompletos) || sePuedeLlegarEscala vuelosCompletos vuelos origen destino


-- EJERCICIO 6


-- Calcula la duración del camino más rápido entre dos ciudades en una agencia de viajes.
-- Considera tanto los vuelos directos como los que incluyen escalas.
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido vuelos origen destino = calcularMinimoLista (listasDuracionVuelosEscala vuelos vuelos origen destino ++ listaDuracionVuelosDirectos vuelos origen destino)
        
calcularMinimoLista :: [Duracion] -> Duracion
calcularMinimoLista [] = 0.0
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
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen [] _ = False
puedoVolverAOrigen vuelos origen = verificarVueloDirecto vuelos vuelos origen || verificarVueloConEscala vuelos vuelos origen

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
