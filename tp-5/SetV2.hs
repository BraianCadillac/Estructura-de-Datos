module SetV2 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

{-
Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

emptyS :: Set a
-PROP: Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a
PROP: Dados un elemento y un conjunto, agrega el elemento al conjunto.

belongs :: Eq a => a -> Set a -> Bool
PROP: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int
PROP: Devuelve la cantidad de elementos distintos de un conjunto.

removeS :: Eq a => a -> Set a -> Set a
PROP: Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a
PROP: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.

setToList :: Eq a => Set a -> [a]
PROP: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.


3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.

-}

data Set a = S [a]


emptyS :: Set a --O(1)
--PROP: Crea un conjunto vacío.
emptyS = S [] 

addS :: Eq a => a -> Set a -> Set a --O(1)
--PROP: Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs) = S (x:xs) 

belongs :: Eq a => a -> Set a -> Bool --O(n) siendo n la longitud de la lista
--PROP: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set a -> Int
--PROP: Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = cantidadDeElementos xs -- O(n²) por cantidadDeElementos

cantidadDeElementos :: Eq a => [a] -> Int --O(n) siendo n la longitud de elementos de a, y por cada elemento hago un llamado a elem. Costo total = O(n²)
cantidadDeElementos []     = 0
cantidadDeElementos (x:xs) = if (elem x xs) --O(n) en el peor caso siendo n la longitud de la lista y está al final
                            then cantidadDeElementos xs
                            else 1 + cantidadDeElementos xs

removeS :: Eq a => a -> Set a -> Set a
--PROP: Borra un elemento del conjunto.
removeS x (S xs) = S (borrar x xs)

borrar :: Eq a => a -> [a] -> [a] --O(n) siendo n la longitud de la lista de a
borrar x []     = []
borrar x (y:ys) = if (x == y)
                    then borrar x ys
                    else y : borrar x ys

unionS :: Eq a => Set a -> Set a -> Set a
--PROP: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs) (S ys) = S (xs++ys) --O(n) siendo n la longitud de la lista xs

setToList :: Eq a => Set a -> [a] -- O(n²) por elementosDe
--PROP: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs) = elementosDe xs

elementosDe :: Eq a => [a] -> [a] --Por cada elemento de la lista de a, hago una operación O(n) siendo n la longitud de xs. Costo total = O(n²)
elementosDe []     = []
elementosDe (x:xs) = if (elem x xs) -- O(n) siendo n la longitud de la lista de xs y en el peor caso x está al final o no está
                        then elementosDe xs
                        else x : elementosDe xs