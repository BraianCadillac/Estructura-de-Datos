module Stack (Stack, emptySt, isEmptyS, push, top, pop, lenS) where

{-
4. Stack (pila)
Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente:

emptySt :: Stack a
Crea una pila vacía.

isEmptyS :: Stack a -> Bool
Dada una pila indica si está vacía.

push :: a -> Stack a -> Stack a
Dados un elemento y una pila, agrega el elemento a la pila.

top :: Stack a -> a
Dada un pila devuelve el elemento del tope de la pila.

pop :: Stack a -> Stack a
Dada una pila devuelve la pila sin el primer elemento.

lenS :: Stack a -> Int
Dada la cantidad de elementos en la pila.
Costo: constante.

2. Implementar el tipo abstracto Stack utilizando una lista.
-}

data Stack a = S [a] Int
    deriving Show
    {-
    INV REPR: En (S xs n)
            *n es la longitud de xs
    -}

emptySt :: Stack a -- O(1)
--Crea una pila vacía.
emptySt = S [] 0

isEmptyS :: Stack a -> Bool -- O(1)
--Dada una pila indica si está vacía.
isEmptyS (S xs n) = n == 0

push :: a -> Stack a -> Stack a -- O(1)
--Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs n) = S (x:xs) (n+1)

top :: Stack a -> a -- O(1)
--Dada un pila devuelve el elemento del tope de la pila.
top (S xs n) = if (n==0)
            then error "No hay elementos en la pila"
            else head xs

pop :: Stack a -> Stack a --O(1)
--Dada una pila devuelve la pila sin el primer elemento.
pop (S xs n) = if (n==0)
            then error "No hay elementos en la pila"
            else S (tail xs) (n-1)

lenS :: Stack a -> Int -- O(1)
--Dada la cantidad de elementos en la pila.
--Costo: constante.
lenS (S xs n) = n