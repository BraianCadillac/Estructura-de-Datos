module MapV1 (Map, emptyM, assocM, lookupM, deleteM, keys) where

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

data Map k v = M [(k, v)]
    {-
    INV REPR: En (M kvs)
                *En kvs no hay claves repetidas
    -}

emptyM :: Map k v --O(1)
--Propósito: devuelve un map vacío
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M (insertar k v kvs) --O(n) por insertar

insertar :: Eq k => k -> v -> [(k, v)] -> [(k, v)] --O(n) siendo n la longitud de la lista de pares k, v. Hace una operación constante por cada elemento de la lista que es el == y el par con un cons :, en el peor caso recorre toda la lista y puede estar al final o no estar entonces lo agrega al final
insertar k v []       = [(k, v)]
insertar k v (kv:kvs) = 
    let (k', v') = kv in
        if (k == k')
        then (k, v) : kvs
        else (k', v') : insertar k v kvs

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = buscarEn k kvs --O(n) por buscarEn

buscarEn :: Eq k => k -> [(k, v)] -> Maybe v --O(n) siendo n la longitud de la lista kvs. Hace unas operaciones constantes por cada elemento de la lista y en el peor caso está al final o puede no estar
buscarEn k []       = Nothing
buscarEn k (kv:kvs) = 
    let (k',v') = kv in
        if (k == k')
        then Just v'
        else buscarEn k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (borrarK k kvs) --O(n) por borrarK

borrarK :: Eq k => k -> [(k, v)] -> [(k, v)] --O(n) siendo n la longitud de la lista. Por cada operación hace una operación O(1) por cada elemento de la lista, en el peor caso puede estar al final la clave que se requiere borrar o no estar
borrarK k []       = []
borrarK k (kv:kvs) = 
    let (k', v') = kv in
        if (k == k')
        then kvs
        else (k', v') : borrarK  k kvs


keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.
keys (M kvs) = claves kvs --O(n) por claves

claves :: [(k, v)] -> [k] --O(n) siendo n la longitud de la lista de claves-valor. Por cada par hace una operación constante O(1) que es por cada clave hace un cons : en la totalidad de la lista
claves []       = []
claves (kv:kvs) = 
    let (k, v) = kv in
        k : claves kvs