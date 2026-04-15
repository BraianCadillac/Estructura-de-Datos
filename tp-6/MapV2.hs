module MapV2 (Map, emptyM, assocM, lookupM, deleteM, keys) where


{-
2. Map (diccionario)
Ejercicio 3
La interfaz del tipo abstracto Map es la siguiente:

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

2. Como una lista de pares-clave valor con claves repetidas
-}

data Map k v = M [(k,v)]

emptyM :: Map k v --O(1)
--Propósito: devuelve un map vacío
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(1)
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M ((k,v):kvs)

lookupM :: Eq k => k -> Map k v -> Maybe v --O(N) por buscar
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = buscar k kvs

buscar :: Eq k => k -> [(k, v)] -> Maybe v --O(N) siendo N la cantidad de claves-valor en la lista. Haciendo una operación constante O(1) por cada elemento de la lista, en el peor caso puede estar al final o no estar
buscar k []       = Nothing
buscar k (kv:kvs) = 
    let (k',v') = kv in
        if (k == k')
        then Just v'
        else buscar k kvs

deleteM :: Eq k => k -> Map k v -> Map k v --O(N) por borrar
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (borrar k kvs)

borrar :: Eq k => k -> [(k, v)] -> [(k, v)] --O(N) siendo N la cantidad de claves de toda la lista. Hace una operación de igualdad y agrega a la lista que es de orden constante O(1) por cada par de la lista
borrar k []       = []
borrar k (kv:kvs) =
    let (k',v') = kv in
        if (k == k')
        then borrar k kvs
        else (k',v') : borrar k kvs

keys :: Map k v -> [k] 
--Propósito: devuelve las claves del map.
keys (M kvs) = claves kvs --O(N²) por claves 

claves :: Eq k => [(k, v)] -> [k] -- O(N²). Por cada k de kv en kvs hago dos operaciones lineal O(N) haciendo un llamado a clavesDe y elem en el peor caso -> O(N) + O(N) = O(N) 
claves []       = []
claves (kv:kvs) = 
    let (k, v) = kv
        ks = clavesDe kvs 
    in
        if (elem k ks)
        then claves kvs
        else k : claves kvs

clavesDe :: [(k,v)] -> [k] --O(n) siendo n la longitud de la lista. Por cada elemento de la lista se hace una operación constante de orden 1 O(1), tanto en abrir la tupla como agregar a la lista
clavesDe []       = []
clavesDe (kv:kvs) = 
    let (k,v) = kv in
        k : clavesDe kvs