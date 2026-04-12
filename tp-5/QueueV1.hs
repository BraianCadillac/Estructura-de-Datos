module QueueV1 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

{-
emptyQ :: Queue a --O(1)
Crea una cola vacía.

isEmptyQ :: Queue a -> Bool --O(1)
Dada una cola indica si la cola está vacía.

enqueue :: a -> Queue a -> Queue a -- O(n)
Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a -- O(1)
Dada una cola devuelve el primer elemento de la cola.

dequeue :: Queue a -> Queue a --O(1)
Dada una cola la devuelve sin su primer elemento.

1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
final de la lista y desencolarse por delante.
-}


data Queue a = Q [a]
    deriving Show

emptyQ :: Queue a -- O(1)
--Crea una cola vacía.
emptyQ = Q []

isEmptyQ :: Queue a -> Bool -- O(1)
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a --O(n) siendo n la longitud de xs y lo agrega al final
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (xs ++ [x])


firstQ :: Queue a -> a --O(1)
--Dada una cola devuelve el primer elemento de la cola.
firstQ (Q xs) = if null xs
                then error "No hay elementos en la cola"
                else head xs


dequeue :: Queue a -> Queue a --O(1)
--Dada una cola la devuelve sin su primer elemento.
dequeue (Q xs) = if null xs
                then error "No hay elementos en la cola"
                else Q (tail xs)


