module MultiSet (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

import MapV1

{-
3. MultiSet (multiconjunto)
Ejercicio 6
Un MultiSet (multiconjunto) es un tipo abstracto de datos similar a un Set (conjunto). A diferencia
del último, cada elemento posee una cantidad de apariciones, que llamaremos ocurrencias del
elemento en el multiset. Su interfaz es la siguiente:

emptyMS :: MultiSet a
Propósito: denota un multiconjunto vacío.

addMS :: Ord a => a -> MultiSet a -> MultiSet a
Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
multiconjunto.

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
elemento en el multiconjunto.

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
ambos multiconjuntos.

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
multiconjuntos tienen en común.

multiSetToList :: MultiSet a -> [(a, Int)]
Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
su cantidad de ocurrencias.

1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
ordenes de complejidad en peor caso de cada función de la interfaz, justificando las respuestas.
-}

{-
emptyM :: Map k v
Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
Propósito: agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v
Propósito: encuentra un valor dado una clave.

deleteM :: Eq k => k -> Map k v -> Map k v
Propósito: borra una asociación dada una clave.

keys :: Map k v -> [k]
Propósito: devuelve las claves del map.

-}

data MultiSet a = MS (Map a Int)

emptyMS :: MultiSet a --O(1)
--Propósito: denota un multiconjunto vacío.
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a --O(n). Siendo n la cantidad de elementos del map, en el peor caso se encuentra al final y luego hace un llamado a assocM de costo O(n) siendo el mismo n en caso de estar -> O(n) + O(n) = O(n)
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS mp) = case (lookupM x mp) of
                Nothing -> MS (assocM x 1 mp)
                Just n  -> MS (assocM x (n+1) mp)

ocurrencesMS :: Ord a => a -> MultiSet a -> Int --O(n) hace un llamado a lookupM de costo O(n) siendo n la cantidad de claves en el map, en el peor caso puede estar al final y recorrer todo el map.
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS x (MS mp) = case (lookupM x mp) of
                        Nothing -> 0
                        Just n  -> n

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a --(opcional) --
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS mp1) (MS mp2) = MS (unionM (keys mp1) mp1 mp2) -- unionM de costo 

unionM :: Ord a => [k] -> Map a Int -> Map a Int -> Map a Int --O(n*(n+m)) por cada n de la lista de a, hago una operación O(n) lookupM + O(m) siendo m la cantidad de claves de mp2, en el peor caso más assocM de costo O(n) siendo n la cantidad de claves en el map. Costo final -> O(n(n+m))
--PRECOND: Todos las claves en ks se encuentran en mp1
unionM [] mp1 mp2     = mp2
unionM (k:ks) mp1 mp2 = case (lookupM k mp1) of
                        Nothing -> error "No se encuentra la clave actual en el map1"
                        Just n  -> case (lookupM k mp2) of
                                    Nothing -> unionM ks mp1 (assocM k n mp2)
                                    Just n' -> unionM ks mp1 (assocM k (n+n') mp2)

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a --(opcional)--O(n⋅(n+m))=O(n²+nm)
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS (MS mp1) (MS mp2) = MS (intersectionM (keys mp1) mp1 mp2)

intersectionM :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int --O(n⋅(n+m))=O(n²+nm). Por cada elemento clave de la lista en el peor caso hace 3 operaciones de costo O(n) (lookupM + lookupM + assocM) siendo n la cantidad de elementos del map mp1, m la cantidad de elementos del map mp2, y assocM de costo O(l) siendo l la cantidad de elementos del map que va creciendo por cada clave de la lista.
intersectionM [] mp1 mp2     = emptyM
intersectionM (k:ks) mp1 mp2 = case (lookupM k mp1) of
                                Nothing -> intersectionM ks mp1 mp2
                                Just n' -> case (lookupM k mp2) of
                                            Nothing -> intersectionM ks mp1 mp2
                                            Just n  -> if n' < n
                                                        then assocM k n' (intersectionM ks mp1 mp2)
                                                        else assocM k n (intersectionM ks mp1 mp2)


multiSetToList :: MultiSet a -> [(a, Int)] --O(n²) + O(n) por keys -> O(n²)
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList (MS mp) = ocurrenciasDe (keys mp) mp

ocurrenciasDe :: Ord a => [a] -> Map a Int -> [(a, Int)] --O(n²). Por cada clave hago una operación de costo lineal O(n) haciendo un llamado a lookupM en el peor caso, sabiendo que todas las claves pertenecen al map. Costo total: O(n²)
ocurrenciasDe [] mp     = []
ocurrenciasDe (k:ks) mp = case (lookupM k mp) of
                        Nothing -> ocurrenciasDe ks mp
                        Just n  -> (k, n) : ocurrenciasDe ks mp