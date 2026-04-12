module QueueV2 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

{-
2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
la eficiencia entre ambas implementaciones.

emptyQ :: Queue a
Crea una cola vacía.

isEmptyQ :: Queue a -> Bool
Dada una cola indica si la cola está vacía.

enqueue :: a -> Queue a -> Queue a
Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a
Dada una cola devuelve el primer elemento de la cola.

dequeue :: Queue a -> Queue a
Dada una cola la devuelve sin su primer elemento.
-}

data Queue a = Q [a]

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (x:xs)

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
firstQ (Q xs) = primerElemento xs --O(n) por primerElemento

dequeue :: Queue a -> Queue a --O(n) orden n por sinElPrimerElemento
--Dada una cola la devuelve sin su primer elemento.
dequeue (Q xs) = Q (sinElPrimerElemento xs)


primerElemento :: [a] -> a --O(n) siendo (n-1) la longitud de la lista, el primer elemento es el último
primerElemento []     = error "No hay elementos en la cola"
primerElemento (x:xs) = if null xs
                        then x
                        else primerElemento xs

sinElPrimerElemento :: [a] -> [a] --O(n) siendo (n-1) la longitud de la lista de a
sinElPrimerElemento []     = error "No hay elementos en la cola"
sinElPrimerElemento (x:xs) = if null xs
                            then []
                            else x : sinElPrimerElemento xs 