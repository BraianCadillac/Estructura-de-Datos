
module MapV3 (Map, emptyM, assocM, lookupM, deleteM, keys) where


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

3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
asociada al valor en la misma posición, pero de la otra lista.
-}

data Map k v = M [k] [v]
    {-
    INV REPR: En (M ks vs)
        *ks y vs tienen la misma longitud
        *En ks no hay k repetidos
    -}

emptyM :: Map k v --O(1)
--Propósito: devuelve un map vacío
emptyM = M [] [] 

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(n) por agregar
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M ks vs) = 
    let (ks', vs') = agregar k v ks vs in
        M ks' vs'


agregar :: Eq k => k -> v -> [k] -> [v] -> ([k], [v]) --O(n) siendo n la longitud de la lista de claves y de la lista de valores. Hace una operación O(1) por cada clave y valor de la lista en el peor caso
--PRECOND: ks y vs tienen la misma longitud
agregar k v [] []             = ([k], [v])
agregar k v (k':ks') (v':vs') = if (k == k')
                                then (k:ks', v:vs')
                                else agregarA k' v'(agregar k v ks' vs')

agregarA :: k -> v -> ([k], [v]) -> ([k], [v]) --O(1)
agregarA k v (ks, vs) = (k:ks, v:vs)

lookupM :: Eq k => k -> Map k v -> Maybe v --O(n) por buscar
--Propósito: encuentra un valor dado una clave.
lookupM k (M ks vs) = buscar k ks vs

buscar :: Eq k => k -> [k] -> [v] -> Maybe v --O(n) siendo n la longitud de la lista de clave y de valores. Haciendo una operación constante O(1) por cada clave, en el peor caso puede estar al final o no estar.
buscar k [] []          = Nothing
buscar k (k':ks) (v:vs) = if (k == k')
                            then Just v
                            else buscar k ks vs

deleteM :: Eq k => k -> Map k v -> Map k v --O(n) por borrar
--Propósito: borra una asociación dada una clave.
deleteM k (M ks vs) = 
    let (ks', vs') = borrar k ks vs in
        M ks' vs'

borrar :: Eq k => k -> [k] -> [v] -> ([k], [v]) --O(n) siendo n la longitud de la lista de claves y valores, en el peor caso puede estar al final o no estar. Hace una operación constante O(1) por cada clave de la lista en ks
borrar k [] []             = ([], [])
borrar k (k':ks') (v':vs') = if (k == k')
                            then (ks', vs')
                            else agregarA k' v' (borrar k ks' vs')

keys :: Map k v -> [k] --O(1)
keys (M ks vs) = ks