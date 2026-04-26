
{-
En dicha representación se observa:
-Un Int, que representa la próxima posición a ocupar en la lista. Es decir, cuando se agregue un elemento al final, debe
agregarse en dicha posición, que luego será incrementada. Cuando la estructura está vacía, el número es 0.

-Un Map Int a, que representa la relación entre índices (claves) y valores de la estructura.

-Una Heap a que contiene todos los valores de la estructura.

a) Definir los invariantes de representación en base a la estructura dada.
-}

data RAList a = MkR Int (Map Int a) (Heap a)
    {-
    INV.REPR: En (MkR n mia ha)
            *n es igual a la cantidad de claves en mia, la cuál es igual a la cantidad de elementos de ha (siendo n >= 0)
            *Los valores de mia y los elementos de ha coinciden considerando multiplicidad
            *Las claves de mia son índices desde 0 hasta (n-1)
    -}


emptyRAL :: RAList a
--Propósito: devuelve una lista vacía.
--Eficiencia: O(1).
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool
--Propósito: indica si la lista está vacía.
--Eficiencia: O(1).
isEmptyRAL (MkR n mia ha) = n == 0

lengthRAL :: RAList a -> Int
--Propósito: devuelve la cantidad de elementos.
--Eficiencia: O(1).
lengthRAL (MkR n mia ha) = n

get :: Int -> RAList a -> a --O(log N) siendo N la cantidad de claves en el map
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
--Eficiencia: O(logN).
get i (MkR n mia ha) = case (lookupM i mia) of
                        Nothing -> error "Índica inválido"
                        Just a  -> a

minRAL :: Ord a => RAList a -> a
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(1).
minRAL (MkR n mia ha) = if isEmptyH ha
                        then error "La lista está vacía"
                        else findMindH ha


add :: Ord a => a -> RAList a -> RAList a
--Propósito: agrega un elemento al final de la lista.
--Eficiencia: O(logN).
add e (MkR n mia ha) = MkR (n+1) (assocM (n) e mia) (insertH e ha)
--assocM = O(log N) siendo N la cantidad de elementos del map
--insertH = O(log N) siendo N la cantidad de elementos de la heap
--Costo de la operación = O(log N + log N) = O(log N)

elems :: Ord a => RAList a -> [a] -- O(N log N) por elemsDesde
--Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
--Eficiencia: O(N log N).
elems (MkR n mia ha) = elemsDesde 0 n mia

elemsDesde :: Int -> Int -> Map Int a -> [a] --Por cada número se hace una operación O(log N), costo de la operación = O(N log N)
--lookupM = O(log N) siendo N la cantidad de elementos del map
--PRECOND: n < m
elemsDesde n m mia = if n == m 
                    then []
                    else case lookupM n mia of
                        Nothing -> error "Índice inválido"
                        Just e  -> e : elemsDesde (n+1) m mia


remove :: Ord a => RAList a -> RAList a -- O(N log N)
--deleteM = O(log N)
--elemento = O(log N)
--borrar = O(N log N)
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(N log N).
remove (MkR n mia ha) = if n == 0
                        then error "La lista está vacía"
                        else MkR (n-1) (deleteM n mia) (borrar (elemento (n-1) mia) ha)

elemento :: Ord a => Int -> Map Int a -> a -- O(log N) siendo N la cantidad de elementos del map
elemento n mia = case lookupM n mia of
                Nothing -> error "No existe la clase"
                Just e  -> e

borrar :: Ord a => a -> Heap a -> Heap a -- En el peor caso por cada elemento de la heap hago dos operaciones O(log N) que es insertH y deleteMinH en la iteración, costo final de la operación = O(N log N)
--isEmptyH = O(1)
--findMindH = O(1)
--deleteMinH = O(log N)
--insertH = O(log N)
borrar e ha = if isEmptyH ha
            then error "El elemento no se encuentra"
            else if e == findMindH ha
                then deleteMinH ha 
                else insertH (findMindH ha) (borrar e (deleteMinH ha))

set :: Ord a => Int -> a -> RAList a -> RAList a -- O(N log N)
--assocM = O(log N) siendo N la cantidad de elementos del map
--reemplazarPor = O(N log N)
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
--Eficiencia: O(N log N).
set m e (MkR n mia ha) = case (lookupM m mia) of
                        Nothing -> error "El índice no existe"
                        Just e' -> MkR n (assocM m e mia) (reemplazarPor e e' ha)

reemplazarPor :: a -> a -> Heap a -> Heap a -- Por cada elemento de la heap hago dos operaciones O(log N) en el peor caso en cada iteración. Costo de la operación = O(N log N)
--isEmptyH = O(1)
--emptyH = O(1)
--findMinH = O(1)
--insertH = O(log N) siendo N la cantidad de elementos de la heap
--deleteMinH = O(log N) siendo N la cantidad de elementos de la heap
reemplazarPor n m ha = if isEmptyH ha
                        then error "Elemento no encontrado"
                        else if (findMindH ha == m)
                            then insertH n (deleteMinH ha)
                            else insertH (findMinH ha) (reemplazarPor n m (deleteMinH ha))

addAt :: Ord a => Int -> a -> RAList a -> RAList a -- O(N log N)
--insertH = O(log N) siendo N la cantidad de elementos de la heap
--correrElementos = O(N log N)
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
--Eficiencia: O(N log N).
--Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
--también como argumento la máxima posición posible.
addAt m e (MkR n mia ha) = if m < 0 || m > n 
                            then error "El índice es inválido"
                            else MkR (n+1) (correrElementos e m n mia) (insertH e ha)

correrElementos :: Ord a => a -> Int -> Int -> Map Int a -> Map Int a --Por cada clave válida hace udos operaciones O(log N) para correr los elementos m+1, costo de la operación = O(N log N)
--lookupM = O(log N) siendo N la cantidad de elementos del map
--assocM = O(log N) siendo N la cantidad de elementos del map
--PRECOND: 0 =< m =< n
correrElementos e m n mia = if m == n
                            then assocM m e mia
                            else case lookupM m mia of
                                    Nothing -> error "El índice es inválido"
                                    Just e' -> 
                                            let mia' =  assocM m e mia in
                                                correrElementos e' (m+1) n mia'