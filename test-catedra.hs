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
    "vuelos no valido porque tienen distinta duracion" ~: vuelosValidos [("Neco", "Mardel", 3.0), ("Neco", "Mardel", 4.0)] ~?= False,
     "vuelo no valido duracion negativa" ~: vuelosValidos [("Buenos Aires", "Córdoba", -2.0)] ~?= False
    ]

testsEjciudadesConectadas = test [
    "ciudad conectada con un elemento" ~: 
        ciudadesConectadas [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"],
    "ciudad conectada con múltiples vuelos" ~:
        ciudadesConectadas [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 3.5)] "Rosario" ~?= ["BsAs", "Cordoba"],
    "ciudad sin conexiones" ~:
        ciudadesConectadas [("BsAs", "Rosario", 5.0)] "Cordoba" ~?= [],
    "ciudad conectada desde ambos lados" ~:
        expectPermutacion (ciudadesConectadas [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 3.5)] "Rosario") ["BsAs"],
    "ciudadesConectadas con varias conexiones duplicadas" ~:
        expectPermutacion (ciudadesConectadas [("Neco", "Mardel", 3.0), ("Mardel", "BsAs", 4.0), ("BsAs", "Neco", 2.0)] "Neco") ["Mardel", "BsAs"],
      "ciudad conectada con posibles rutas alternativas" ~:
        expectAny (ciudadesConectadas [("Neco", "Mardel", 3.0), ("Mardel", "Neco", 3.0)] "Neco") [["Mardel"], ["BsAs", "Mardel"]]
    ]

testsEjmodernizarFlota = test [
    "flota modernizada sin elementos" ~: 
        modernizarFlota [] ~?= [] ,

    "flota modernizada con un elemento" ~: 
        expectlistProximity 
            [dur | (_, _, dur) <- modernizarFlota [("Azul", "Bahia Blanca", 5.19)]]
            [4.671],

    "flota modernizada con dos elementos" ~: 
        expectlistProximity 
            [dur | (_, _, dur) <- modernizarFlota 
                [("Azul", "Bahia Blanca", 5.19), ("Necochea", "Mar del Plata", 3.0)]]
            [4.671, 2.7],

    "flota modernizada con tres elementos" ~: 
        expectlistProximity 
            [dur | (_, _, dur) <- modernizarFlota 
                [("Azul", "Bahia Blanca", 5.19), 
                 ("Necochea", "Mar del Plata", 3.0), 
                 ("Cataratas", "Rio Grande", 12.0)]]
            [4.671, 2.7, 10.8]
    ]



testsEjciudadMasConectada = test [
   "ciudad más conectada con dos vuelos diferentes" ~: 
        ciudadMasConectada [("Buenos Aires", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario",

    "ciudad más conectada con múltiples vuelos" ~:
        ciudadMasConectada [("Buenos Aires", "Rosario", 5.0), ("Rosario", "Córdoba", 3.5), ("Córdoba", "Mendoza", 4.0), ("Rosario", "Mendoza", 2.5)] ~?= "Rosario",
    
    "ciudad central con conexiones de varios puntos" ~:
        ciudadMasConectada [("Mendoza", "Buenos Aires", 6.0), ("Córdoba", "Buenos Aires", 5.0), ("Rosario", "Buenos Aires", 4.5)] ~?= "Buenos Aires",

    "ciudad sin conexiones (lista vacía)" ~:
        ciudadMasConectada [] ~?= [],
    "ciudad más conectada entre múltiples opciones válidas" ~:
        expectAny (ciudadMasConectada [("Buenos Aires", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Buenos Aires", "Córdoba", 3.0)]) ["Buenos Aires", "Rosario", "Córdoba"]
    ]


testsEjsePuedeLlegar = test [
    "Se puede llegar sin vuelos" ~: sePuedeLlegar [] "BsAs" "Mendoza" ~?= False,
    "Se puede llegar directo" ~: sePuedeLlegar [("BsAs", "Mendoza", 5.0)] "BsAs" "Mendoza" ~?= True,
    "Se puede llegar caso verdadero con una escala" ~: 
        sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True,
    "Se puede llegar con escala incluso si hay vuelos que estorban de por medio" ~: 
        sePuedeLlegar [("Necochea", "Bahia Blanca", 5.0), ("Rosario", "BsAs", 4.0), ("Bahia Blanca", "BsAs", 6.0)] "Necochea" "BsAs" ~?= True,
    "Se puede llegar directo y con escala" ~: 
        sePuedeLlegar [("Mar del Plata", "Rio Grande", 10.0), ("Pinamar", "Miramar", 1.0), ("Rio Grande", "Miramar", 9.0), ("Mar del Plata", "Miramar", 1.0)] "Mar del Plata" "Miramar" ~?= True,
    "Se puede llegar con escala iterando" ~: 
        sePuedeLlegar [("Tucuman", "Corrientes", 4.0), ("Tucuman", "Entre Rios", 3.0), ("Entre Rios", "BsAs", 5.0)] "Tucuman" "BsAs" ~?= True,
    "Se puede llegar caso falso porque necesito más de una escala" ~: 
        sePuedeLlegar [("Miramar", "Pinamar", 1.0), ("Pinamar", "Mendoza", 3.0), ("Mendoza", "Cataratas", 6.0), ("Cataratas", "Cordoba", 5.0)] "Miramar" "Córdoba" ~?= False,
    "Se puede llegar a caso verdadero directo o con escala" ~: 
        sePuedeLlegar [("Tucuman", "Corrientes", 4.0), ("Mar del Plata", "Entre Rios", 3.0), ("Entre Rios", "BsAs", 5.0)] "Tucuman" "BsAs" ~?= False
    ]


testsEjduracionDelCaminoMasRapido = test [
        "duracion del camino mas rapido con un solo vuelo directo" ~: duracionDelCaminoMasRapido [("Necochea", "Mar del Plata", 2.0)] "Necochea" "Mar del Plata" ~?= 2.0 ,
        "duracion del camino mas rapido con un vuelo directo y uno con escala(gana directo)" ~: duracionDelCaminoMasRapido [("Neuquen", "Sta Rosa", 4.0), ("Sta Rosa", "Trelew", 3.0), ("Neuquen", "Trelew", 2.0)] "Neuquen" "Trelew" ~?= 2.0 ,
        "duracion del camino mas rapido con un vuelo directo y uno con escala(gana escala)" ~: duracionDelCaminoMasRapido [("Neuquen", "Sta Rosa", 1.0), ("Sta Rosa", "Trelew", 2.0), ("Neuquen", "Trelew", 5.0)] "Neuquen" "Trelew" ~?= 3.0,
        "duración del camino más rápido con varias rutas válidas" ~: expectAny (duracionDelCaminoMasRapido [("BsAs", "Rosario", 3.0), ("BsAs", "Tandil", 2.0), ("Tandil", "Córdoba", 2.5), ("Rosario", "Córdoba", 4.0)] "BsAs" "Córdoba") [5.0, 6.0]
    ]

testsEjpuedoVolverAOrigen = test [
        "puedo volver a origen con un vuelo" ~: puedoVolverAOrigen [("BsAs", "Mar del Plata", 4.0)] "BsAs" ~?= False ,
        "puedo volver a origen caso verdadero directo" ~: puedoVolverAOrigen [("BsAs", "Necochea", 2.0), ("Necochea", "BsAs", 2.0)] "BsAs" ~?= True ,
        "puedo volver a origen caso verdadero directo por mas de que haya otros viajes de por medio" ~: -- la recursión busca hasta encontrar un viaje que cumpla
          puedoVolverAOrigen [("Necochea", "Tandil", 5.0), ("BsAs", "Necochea", 2.0), ("Tandil", "Azul", 2.0), ("Necochea", "BsAs", 2.0)] "BsAs" ~?= True ,
        "puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Cordoba", "Mendoza", 3.0), ("Mendoza", "Rosario", 8.0)] "Rosario" ~?= True ,
        "puedo volver a origen caso verdadero con más de una escala" ~: 
          puedoVolverAOrigen [("BsAs", "Rosario", 3.0), ("Rosario", "Córdoba", 2.5), ("Córdoba", "Neuquén", 3.0), ("Neuquén", "BsAs", 4.0)] "BsAs" ~?= True ,
        "puedo volver a origen caso falso en general" ~: -- puedo conectar varios viajes entre si y volver a otras ciudades pero ninguno me va a devolver a la ciudad de la que sali (BsAs)
          puedoVolverAOrigen [("Madrid", "Roma", 5.0), ("Roma", "Madrid", 5.0), ("BsAs", "Mar del Plata", 4.0), ("Necochea", "Roma", 12.0), ("Mar del Plata", "Necochea", 2.0)] "BsAs" ~?= False
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