module Solucion  where
import Test.HUnit
    ( (~:), (~?=), runTestTT, Counts, Test, Testable(test) )

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

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



testsEjciudadesConectadas :: Test
testsEjciudadesConectadas = test [
    "ciudad conectada con un elemento" ~: 
        ciudadesConectadas [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"],
    
    "ciudad conectada con múltiples vuelos" ~:
        ciudadesConectadas [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 3.5)] "Rosario" ~?= ["BsAs", "Cordoba"],
    
    "ciudad sin conexiones" ~:
        ciudadesConectadas [("BsAs", "Rosario", 5.0)] "Cordoba" ~?= [],

    "ciudad conectada desde ambos lados" ~:
        ciudadesConectadas [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 3.5)] "Rosario" ~?= ["BsAs"]
    ]


main :: IO Counts
main = runTestTT testsEjciudadesConectadas
