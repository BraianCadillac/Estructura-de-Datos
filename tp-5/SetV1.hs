module SetV1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

{-
Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

emptyS :: Set a -- O(1)
-PROP: Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a -- O(n)
PROP: Dados un elemento y un conjunto, agrega el elemento al conjunto.

belongs :: Eq a => a -> Set a -> Bool -- O(n)
PROP: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int -- O(1)
PROP: Devuelve la cantidad de elementos distintos de un conjunto.

removeS :: Eq a => a -> Set a -> Set a -- O(n)
PROP: Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a O(n²)
PROP: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.

setToList :: Eq a => Set a -> [a] -- O(1)
PROP: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.


1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
la cantidad de elementos en la estructura.
Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
de esta implementación, pero para mantener una interfaz común entre distintas posibles
implementaciones estamos obligados a escribir así los tipos.

-}

data Set a = S [a] Int
    deriving Show
    {-
    INV REPR: siendo (S xs n)
        *n es igual a la longitud de xs
        *xs no contiene elementos repetidos
    -}

emptyS :: Set a --O(1)
--PROP: Crea un conjunto vacío.
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a --O(n) siendo n la cantidad de elementos del conjunto, puede estar al final en el peor caso
--PROP: Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = if (elem x xs) 
                then S xs n
                else S (x:xs) (n+1)

belongs :: Eq a => a -> Set a -> Bool --O(n) siendo n en el peor caso la longitud de los elementos del conjunto
--PROP: Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs n) = elem x xs

sizeS :: Eq a => Set a -> Int --O(1)
--PROP: Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs n) = n

removeS :: Eq a => a -> Set a -> Set a --O(n+n) = O(n) siendo n la cantidad de elementos del set
--PROP: Borra un elemento del conjunto.
removeS x (S xs n) = if (elem x xs) -- O(n) siendo n la longitud de la lista xs y en el peor caso x puede estar al final
                    then S (borrarElemento x xs) (n-1)
                    else S xs n

borrarElemento :: Eq a => a -> [a] -> [a] --O(n) siendo n la longitud de la lista. En el peor caso puede estar al final
borrarElemento x []     = []
borrarElemento x (y:ys) = if (x == y) 
                            then ys
                            else y : borrarElemento x ys

unionS :: Eq a => Set a -> Set a -> Set a
--PROP: Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs n) (S ys m) = 
    let l = (unirListas xs ys) in
        S l (length l) -- O(n² + n) = siendo n la longitud de la lista, n es absorvido por lo tanto queda = O(n²)

unirListas :: Eq a => [a] -> [a] -> [a] --O(n²) porque por cada elemento de xs hago una operación O(n) de la longitud de ys
unirListas [] ys     = ys
unirListas (x:xs) ys = if (elem x ys) -- O(n) siendo n la longitud de ys. En el peor caso x puede estar al final
                        then unirListas xs ys
                        else x : unirListas xs ys

setToList :: Eq a => Set a -> [a]
--PROP: Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs n) = xs