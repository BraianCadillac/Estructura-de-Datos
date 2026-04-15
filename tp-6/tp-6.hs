import PriorityQueue
import MapV1
import MultiSet

{-
emptyPQ :: PriorityQueue a --O(1)
Propósito: devuelve una priority queue vacía.

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
Propósito: indica si la priority queue está vacía.

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(n)
Propósito: inserta un elemento en la priority queue.

findMinPQ :: Ord a => PriorityQueue a -> a --O(1)
Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
Precondición: parcial en caso de priority queue vacía.

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(1)
Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
Precondición: parcial en caso de priority queue vacía.
-}


{-
Ejercicio 2
Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¿Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
práctica.
-}



heapSort :: Ord a => [a] -> [a] -- ordenado O(n) + armarPQ O(n²) = O(n²)
heapSort xs = ordenado (armarPQ xs)

armarPQ :: Ord a => [a] -> PriorityQueue a -- O(n²). Hace una operación O(n) por cada elemento de la lista haciendo un llamado a insertPQ siendo n la cantidad de elementos de la cola con prioridad
armarPQ []     = emptyPQ
armarPQ (x:xs) = insertPQ x (armarPQ xs)

ordenado :: Ord a => PriorityQueue a -> [a] --O(n) siendo n la cantidad de elementos de la cola con prioridad. Haciendo una operación constante O(1) haciendo un llamado a findMinPQ + isEmptyPQ O(1) por cada elemento en la recursión
ordenado pq = if isEmptyPQ pq
                then []
                else findMinPQ pq : (ordenado (deleteMinPQ pq))


-------------------------------------------------------------------------------




{-
Implementar como usuario del tipo abstracto Map las siguientes funciones:

1. valuesM :: Eq k => Map k v -> [Maybe v]
Propósito: obtiene los valores asociados a cada clave del map.

2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
Propósito: indica si en el map se encuentran todas las claves dadas.

3. listToMap :: Eq k => [(k, v)] -> Map k v
Propósito: convierte una lista de pares clave valor en un map.

4. mapToList :: Eq k => Map k v -> [(k, v)]
Propósito: convierte un map en una lista de pares clave valor.

5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
la misma clave.

6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.

7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.
Indicar los ordenes
-}

{-
emptyM :: Map k v --o(1)
Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(n)
Propósito: agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v --O(n)
Propósito: encuentra un valor dado una clave.

deleteM :: Eq k => k -> Map k v -> Map k v --O(n)
Propósito: borra una asociación dada una clave.

keys :: Map k v -> [k] --O(n)
Propósito: devuelve las claves del map.
-}

valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM mp = valores (keys mp) mp -- --O(n²) + O(n) = --O(n²)

valores :: Eq k => [k] -> Map k v -> [Maybe v] --O(n²) siendo n la longitud de la lista de ks, por cada operación en cada clave de la lista se hace una operación lookupM de O(n) siendo n la cantidad de claves en el mp (siendo el mismo n). Por lo tanto O(n²) 
valores [] mp     = []
valores (k:ks) mp = lookupM k mp : valores ks mp

todasAsociadas :: Eq k => [k] -> Map k v -> Bool --O(n·m), siendo n la cantidad de claves a buscar y m la cantidad de claves del map. En el peor caso, n = m ⇒ O(n²).
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] mp     = True
todasAsociadas (k:ks) mp = case (lookupM k mp) of
                            Nothing -> False
                            Just v  -> todasAsociadas ks mp

listToMap :: Eq k => [(k, v)] -> Map k v --O(n²). Por cada elemento par de la lista hago una operación O(n) siendo n la cantidad de elementos en el map, y por cada operación el map crece -> O(n²)
--Propósito: convierte una lista de pares clave valor en un map.
listToMap []       = emptyM
listToMap (kv:kvs) = let (k, v) = kv in
    assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList mp = listaDePares (keys mp) (valuesM mp) -- O(n) + O(n) + O(n²) = O(n²)

listaDePares :: [k] -> [Maybe v] -> [(k, v)] --O(n) siendo n la longitud de ambas listas. Por cada operación hago un par y se usa el cons :
--PRECOND: Para cada k en ks, tiene un valor válido en vs. Tienen la misma longitud
listaDePares [] []         = []
listaDePares (k:ks) (v:vs) = (k, fromJust v) : listaDePares ks vs

agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq []       = emptyM
agruparEq (kv:kvs) = 
    let (k, v) = kv in
        agrupar k v (agruparEq kvs)

agrupar :: Eq k => k -> v -> Map k [v] -- O(n) siendo n la cantidad claves-valor en el map. En el peor caso la clave puede estar al final o no estar, haciendo un llamado a lookupM y a assocM -> O(n) + O(n) = O(n)
agrupar k v mp = case (lookupM k mp) of
                Nothing -> assocM k [v] mp
                Just vs -> assocM k (v:vs) mp

incrementar :: Eq k => [k] -> Map k Int -> Map k Int --O(n²). En el peor cada k en ks hace una operación O(n) siendo n la cantidad de claves-valor y assocM O(n) siendo el mismo n -> O(n) + O(n) = O(n) por lo tanto incrementar cuesta O(n²)
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar [] mp     = mp
incrementar (k:ks) mp = case (lookupM k mp) of
                        Nothing -> incrementar ks mp
                        Just n  -> incrementar ks (assocM k (n+1) mp)

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero. Indicar los ordenes
mergeMaps mp1 mp2 = agregarClavesValor (keys mp1) (valuesM mp1) mp2

agregarClavesValor :: Eq k => [k] -> [Maybe v] -> Map k v -> Map k v --O(n²). Por cada clave-valor hace una operación O(n) siendo n la cantidad de claves-valores que se encuentran en el map y el map por cada operación va creciendo.
--lookupM -> O(n)
--AssocM  -> O(n)
--O(n) + O(n) = O(n)
--PRECOND: ks y vs tienen la misma longitud y cada v se corresponde para cada k.
agregarClavesValor [] [] mp         = mp
agregarClavesValor (k:ks) (v:vs) mp = case (lookupM k mp) of
                                        Nothing -> agregarClavesValor ks vs (assocM k (fromJust v) mp)
                                        Just v' -> agregarClavesValor ks vs (assocM k (fromJust v) mp)


{-
Ejercicio 5
Implemente estas otras funciones como usuario de Map:

indexar :: [a] -> Map Int a
Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
su posición en la lista.

ocurrencias :: String -> Map Char Int
Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
en el string, y los valores la cantidad de veces que aparecen en el mismo.

Indicar los ordenes de complejidad en peor caso de cada función del usuario en base a la
implementación elegida, justificando las respuestas.
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

1. Como una lista de pares-clave valor sin claves repetidas
-}

indexar :: [a] -> Map Int a --O(n²) por indexarDesde
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
indexar xs = indexarDesde 0 xs

indexarDesde :: Int -> [a] -> Map Int a --O(n²). Por cada elemento de la lista de a, hace una operación llamada assocM de órden lineal O(n) del tamaño del map, por cada elemento de la lista el map va creciendo. 
indexarDesde n []     = emptyM
indexarDesde n (x:xs) = assocM n x (indexarDesde (n+1) xs)

ocurrencias :: String -> Map Char Int --O(n²) por cada caracter hace una operación O(n) haciendo un llamado a agregar. Por cada carácter el map crece por lo que -> O(n²)
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias []     = emptyM
ocurrencias (c:cs) = agregar c (ocurrencias cs)

agregar :: Char -> Map Char Int -> Map Char Int --O(n) siendo n la cantidad de claves del map hace un llamado a lookupM, en el peor caso hace un llamado a assocM donde la clave se encuentra al final del map -> O(n) + O(n) = O(n)
agregar c mp = case (lookupM c mp) of
                Nothing -> assocM c 1 mp
                Just n  -> assocM c (n+1) mp


{-


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

{-2. Reimplementar como usuario de MultiSet la función ocurrencias de ejercicios anteriores,
que dado un string cuenta la cantidad de ocurrencias de cada caracter en el string. En este
caso el resultado será un multiconjunto de caracteres.-}

ocurrencias' :: String -> MultiSet Char -- O(n²) por cada caracter hago una operación O(n) siendo n la cantidad de elementos en el multiset y por cada caracter, el multiset va creciendo
ocurrencias' []     = emptyMS
ocurrencias' (c:cs) = addMS c (ocurrencias' cs)