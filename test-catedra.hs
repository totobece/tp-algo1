import Test.HUnit
import Data.List
import Solucion
-- No está permitido agregar nuevos imports.


runCatedraTests = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    "vuelo no valido duracion 0" ~: vuelosValidos [("Cataratas", "Rio Grande", 0)] ~?= False,
    "vuelos valido sin elementos" ~: vuelosValidos [] ~?= False,
    "vuelos valido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True,
    "vuelos no valido con un elemento" ~: vuelosValidos [("BsAs", "BsAs", 1.0)] ~?= False,
    "vuelos valido con dos elementos" ~: vuelosValidos [("Neco", "Mardel", 3.0), ("Rosario", "BsAs", 4.0)] ~?= True,
    "vuelos no valido con dos elementos" ~: vuelosValidos [("Neco", "Mardel", 3.0), ("Neco", "Mardel", 3.0)] ~?= False,
    "vuelos no valido porque tienen distinta duracion" ~: vuelosValidos [("Neco", "Mardel", 3.0), ("Neco", "Mardel", 4.0)] ~?= False
    ]

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

testsEjmodernizarFlota = test [
    "flota modernizada sin elementos" ~: 
        modernizarFlota [] ~?= [],
    "flota modernizada con un elemento" ~:
        modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)],
    "flota modernizada con dos elementos" ~:
        modernizarFlota [("BsAs", "Rosario", 4.0), ("Necochea", "Mar del Plata", 2.0)] ~?= [("BsAs", "Rosario", 3.6), ("Necochea", "Mar del Plata", 1.8)],
    "flota modernizada con tres elementos" ~:
        modernizarFlota [("BsAs", "Rosario", 12.0), ("Necochea", "Mar del Plata", 3.0), ("Cataratas", "Rio Grande", 5.19)] ~?= [("BsAs", "Rosario", 10.8), ("Necochea", "Mar del Plata", 2.7), ("Cataratas", "Rio Grande", 4.671)]
    ]

testsEjciudadMasConectada = test [
    "ciudad Mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario"
    ]

testsEjsePuedeLlegar = test [
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True
    ]

testsEjduracionDelCaminoMasRapido = test [
    "duración del camino más rápido con una escala" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= 10.0
    ]

testsEjpuedoVolverAOrigen = test [
        "puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" ~?= True
    ]



-- Funciones extras

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)