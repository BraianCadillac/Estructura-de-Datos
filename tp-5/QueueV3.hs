module QueueV3 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

{-
5. Queue con dos listas
Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).
La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
a la otra bs (back stack). 

Quitaremos elementos a través de fs y agregaremos a través de bs, pero
todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
vacía, entonces la cola se encuentra vacía.

¿Qué ventaja tiene esta representación de Queue con respecto a la que usa una sola lista?
Que es más barato las operaciones pero tiene un costo amortizado que se usa una sola vez cada tanto
-}

data Queue a = Q [a] [a]
    {-
    INV REPR: En (Q fs bs)
            * Si fs se encuentra vacía, entonces la cola se encuentra vacía
    -}

emptyQ :: Queue a --O(1)
--Crea una cola vacía.
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool --O(1)
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q fs bs) = null fs

enqueue :: a -> Queue a -> Queue a -- O(1)
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q fs bs) = if null fs
                        then Q (x:fs) bs
                        else Q fs (x:bs)

firstQ :: Queue a -> a -- O(1)
--Dada una cola devuelve el primer elemento de la cola.
firstQ (Q fs bs) = if null fs
                    then error "No hay elementos en la cola"
                    else head fs

dequeue :: Queue a -> Queue a --O(1) en la mayoría de los casos pero es O(n) amortizado, o sea no siempre se va a ejecutar el reverse
--Dada una cola la devuelve sin su primer elemento.
dequeue (Q fs bs) = if null fs
                    then error "No hay elementos en la cola"
                    else if (null (tail fs))
                        then Q (reverse bs) []
                        else Q (tail fs) bs 