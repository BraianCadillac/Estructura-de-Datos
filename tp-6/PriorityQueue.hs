module PriorityQueue (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

{-
1. Priority Queue (cola de prioridad)
Ejercicio 1
La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.

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

data PriorityQueue a = PQ [a]
    {-
    INV REPR: En (PQ xs)
        *xs está ordenado de menor a mayor
    -}

emptyPQ :: PriorityQueue a -- O(1)
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(n) siendo n la longitud de la lista en el peor caso se agrega el elemento a la cola con prioridad al final
--Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ xs) = PQ (insertarEn x xs)

insertarEn :: Ord a => a -> [a] -> [a]
insertarEn x []     = x : []
insertarEn x (y:ys) = if (x < y)
                        then x : y : ys 
                        else y : insertarEn x ys

findMinPQ :: Ord a => PriorityQueue a -> a --O(1)
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = if null xs
                    then error "No hay elementos en la cola con prioridad"
                    else head xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(1)
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = if null xs
                    then error "No hay elementos en la cola con prioridad"
                    else PQ (tail xs)