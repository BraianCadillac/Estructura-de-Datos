estaOrdenada :: Ord a => [a] -> Bool -- O(n) donde por cada n de los elementos de a de la lista, se hace una operación constante por cada iteración
--PROPÓSITO: Indica si una lista está ordenada de menor a mayor.
estaOrdenada []     = True
estaOrdenada (x:xs) = esMenor x xs && estaOrdenada xs

esMenor :: Ord a => a -> [a] -> Bool --O(1)
esMenor x []    = True
esMenor x (y:_) = x <= y

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

podar :: Eq a => a -> Tree a -> Tree a --O(n) donde n es la cantidad total de nodos en el árbol
--Propósito: Elimina los subarboles cuya raiz es igual al elemento dado.
podar x EmptyT          = EmptyT
podar x (NodeT y ti td) = if (x==y)
                        then EmptyT
                        else NodeT y (podar x ti) (podar x td)


data AppendList a = Nil | Unit a | Append (AppendList a) (AppendList a)

toList :: AppendList a -> [a]
toList Nil              = []
toList (Unit x)         = [x]
toList (Append ali ald) = (toList ali) ++ (toList ald)

sum :: AppendList Int -> Int
sum Nil              = 0
sum (Unit n)         = n
sum (Append ali ald) = (sum ali) + (sum ald)


--Ejercicio 4 
--PROPÓSITO: Dada una lista, comprime los elementos consecutivos que son iguales, creando una lista donde cada elemento indica la cantidad de veces que aparece de forma consecutiva. Se debe respetar el orden de los elementos en la lista.

comprimir :: Eq a => [a] -> [(Int, a)]
comprimir []     = []
comprimir (x:xs) = juntar (1, x) (comprimir xs)

juntar :: Eq a => (Int, a) -> [(Int, a)] -> [(Int, a)]
juntar ix [] = [ix]
juntar ix (iy:iys) = let (i,x) = ix
                    (i',y) = iy in 
                        if x == y
                        then (i+i',x) : iys
                        else ix : iy : iys

descomprimir :: [(Int, a)] -> [a]
descomprimir []       = []
descomprimir (ia:ias) = let (i,a) = ia in
    descomprimirPar i a ++ (descomprimir ias)

descomprimirPar :: Int -> a -> [a]
descomprimirPar 0 x = []
descomprimirPar n x = x : descomprimirPar (n-1) x

