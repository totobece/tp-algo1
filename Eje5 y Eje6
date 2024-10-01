module TestSolucion where


type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

-- FUNCION DEL EJERCICIO 1 PARA EJECUTAR EL 5 Y 6

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vs) = vueloValido v && sinVuelosDuplicados (v:vs) && vuelosValidos vs 

vueloValido :: Vuelo -> Bool
vueloValido (ciudadOrigen, ciudadDestino, duracionViaje) = duracionViaje > 0 && ciudadOrigen /= ciudadDestino

verificarMismoVuelo :: Vuelo -> Vuelo -> Bool
verificarMismoVuelo (ciudadOrigen1, ciudadDestino1, _) (ciudadOrigen2, ciudadDestino2, _) = ciudadOrigen1 == ciudadOrigen2 && ciudadDestino1 == ciudadDestino2

sinVuelosDuplicados :: [Vuelo] -> Bool
sinVuelosDuplicados [] = True
sinVuelosDuplicados (v:vs) = not (any(verificarMismoVuelo v) vs) && sinVuelosDuplicados vs

---- EJERCICIO 5 ----  

-- Esta funcion tiene que evaluar que si le damos una lista de vuelos, del tipo (Cordoba,BsAs,5.0), un origen y un destino, 
--debe existir al menos una forma de llegar al destino partiendo desde el origen y teniendo en cuenta que el vuelo puede ser 
--directo o con maximo una escala.

--Ejemplo
-- sePuedeLlegar = [("Cordoba","BsAs",5.0)] "Cordoba" "BsAs"
-- Esto devuelve True porque la lista tiene una tupla que cumple con la condicion de que partimos desde Cordoba y llegamos a BsAs directamente. 

-- Esta funcion primero determina si los vuelos son validos, en caso de que no lo sean devuelve falso y en caso de que lo sean
-- llama a dos funciones evaluadas por una disyuncion que determina si el vuelo es directo o con escalas. Si estas no se cumplen devuelve falso.

sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino
    | vuelosValidos(vuelos) = (sePuedeLlegarDirecto vuelos origen destino) || (sePuedeLlegarEscala vuelos vuelos origen destino)
    | otherwise = False

--Estas funciones determinan si existe un vuelo tal que el origen sea igual que el primer elemento de el primer vuelo y que el destino sea igual 
--al segundo elemento. Si esto sucede quiere decir que hay un vuelo que es directo.

sePuedeLlegarDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegarDirecto [] _ _ = False
sePuedeLlegarDirecto (vuelo:vuelos) origen destino = (comparaPuntos vuelo origen destino) || (sePuedeLlegarDirecto vuelos origen destino)
    where 
        comparaPuntos :: (Vuelo) -> Ciudad -> Ciudad -> Bool
        comparaPuntos (partida,llegada,_) origen destino = (partida == origen) && (llegada == destino)

--Estas funciones determinan si existe un vuelo tal que el origen sea igual que el primer elemento de un vuelo, si es igual llama a otra funcion
--la cual compara si existe el segundo elemento del vuelo posicionado como el primer elemento de uno de los vuelos restantes, si lo encuentra 
--consulta si el ultimo elemento de ese mismo vuelo es igual al destino que estamos buscando.
--Esta funcion toma la lista completa de vuelos para iterar en todos.

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

---- EJERCICIO 6 ----   

duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido vuelos origen destino = calcularMinimoLista (listasDuracionVuelosEscala vuelos vuelos origen destino ++ listaDuracionVuelosDirectos vuelos origen destino)
    where
        calcularMinimoLista :: [Duracion] -> Duracion
        calcularMinimoLista [x] = x
        calcularMinimoLista (x:xs)
            | x < calcularMinimoLista xs = x
            | otherwise = calcularMinimoLista xs

--duracionDelCaminoMasRapido [("BsAs","Neuquen",7.0),("BsAs","Tucuman",4.0),("Neuquen","Jujuy",7.0),("Tucuman","Jujuy",3.0),("BsAs","Jujuy",5.0)] "BsAs" "Jujuy" 
--("BsAs","Tucuman",4.0),("Tucuman","Jujuy",3.0) = 7.0
--("BsAs","Neuquen",7.0),("Neuquen","Jujuy",7.0) = 14.0

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

