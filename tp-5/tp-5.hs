
import SetV1
import QueueV1
import Stack

{-1.

Cálculo de costos
Especificar el costo operacional de las siguientes funciones:

head' :: [a] -> a --O(1) costo constante
head' (x:xs) = x

sumar :: Int -> Int --O(1) costo constante
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int -> Int --O(n) siendo n la cantidad de llamados recursivos y por cada operación hace un trabajo de costo constante.
factorial 0 = 1
factorial n = n * factorial (n-1) 

longitud :: [a] -> Int -- La función recorre la lista elemento por elemento, realizando una llamada recursiva por cada uno. Como la lista tiene longitud n y en cada paso se realiza una cantidad constante de operaciones, el costo total es O(n).
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factoriales :: [Int] -> [Int] -- O(n) siendo n la longitud de la lista. En el peor caso, cada elemento de la lista es del orden de n, por lo que calcular cada factorial cuesta O(n). Como hay n elementos, el costo total es O(n·n) = O(n²).
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

pertenece :: Eq a => a -> [a] -> Bool -- En el peor caso es O(n) siendo n la longitud de la lista y el elemento dado puede estar al final o no estar
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

sinRepetidos :: Eq a => [a] -> [a] -- O(n) siendo n la longitud de la lista, y por cada operación en el peor caso hace un llamado a pertenece que cuesta O(n) siendo n la longitud de esa lista. Costo final : O(n·n) = O(n²)
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if pertenece x xs --O(n)
    then sinRepetidos xs
    else x : sinRepetidos xs --O(1)

-- equivalente a (++)
append :: [a] -> [a] -> [a] -- O(n) siendo n la longitud de la lista xs 
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String -- Sea n la cantidad de strings y m la longitud total de los caracteres. El costo es O(m). En el peor caso, si cada string tiene longitud O(n), entonces m = O(n²), y el costo total es O(n²).
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a] --O(n) siendo n la cantidad de llamados recursivos y en el peor caso el mismo n de la longitud de la lista
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a] -- O(n) siendo n la cantidad de llamados recursivos y en el peor caso el mismo n de la longitud de la lista
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int -> [a] -> ([a], [a]) -- O(n + n) = O(n) siendo n la cantidad de llamados entre takeN y dropN
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a -- O(n) en el peor caso siendo n la longitud de xs haciendo una operación por cada elemento de la lista de costo O(1)
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a] -- O(n) en el peor caso siendo n la longitud de xs, podría estar el elemento al final o no estar
sacar n [] = []
sacar n (x:xs) =
    if n == x
    then xs
    else x : sacar n xs

ordenar :: Ord a => [a] -> [a] -- O(n) siendo n la longitud de la lista, y en peor caso por cada elemento de la lista hace una operación de costo lineal de orden n, que en este caso en peor caso el minimo es el último elemento. Costo total = O(n·n) = O(n²).
ordenar [] = []
ordenar xs =
    let m = minimo xs
    in m : ordenar (sacar m xs)

-}










{-
2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
al conjunto.

sinRepetidos :: Eq a => [a] -> [a]
Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura
auxiliar.

unirTodos :: Eq a => Tree (Set a) -> Set a
Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
del arbol.
-}

{-
emptyS :: Set a
PROP: Crea un conjunto vacío.

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
-}

losQuePertenecen :: Eq a => [a] -> Set a -> [a] -- O(n) siendo n la longitud de la lista. Y en el peor caso por cada elemento de la lista hago una operación O(m) haciendo un llamado a belongs en el peor caso recorre todos los elementos del set
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto, costo total = O(n·m)
losQuePertenecen [] s     = []
losQuePertenecen (x:xs) s = if (belongs x s)
                            then x : losQuePertenecen xs s
                            else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (agregarElementosAlSet xs) -- O(1 + n·m) = O(n·m)

agregarElementosAlSet :: Eq a => [a] -> Set a --O(n) siendo n la longitud de la lista, y por cada elemento de la lista hago una operación O(n) siendo n la cantidad de elementos del set. El costo es cuadrático porque cada inserción recorre un conjunto cuyo tamaño crece linealmente. 
--Costo total = O(n²)
agregarElementosAlSet []     = emptyS
agregarElementosAlSet (x:xs) = addS x (agregarElementosAlSet xs)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

unirTodos :: Eq a => Tree (Set a) -> Set a -- O(k · n²)
-- donde k es la cantidad de nodos del árbol y n la cantidad de elementos de cada Set.
-- Por cada nodo del árbol se realiza una operación unionS de costo O(n²).
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos EmptyT          = emptyS
unirTodos (NodeT s si sd) = unionS s (unionS (unirTodos si) (unirTodos sd))


{-
3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:


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
-}

lengthQ :: Queue a -> Int --O(n) siendo n la cantidad de elementos de la cola, por cada elemento de la cola se hacen operaciones O(1) en el peor caso
--isEmptyQ O(1) 
--dequeue O(1) 
--O(1) + O(1)
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if (isEmptyQ q)
            then 0
            else 1 + lengthQ (dequeue q)



queueToList :: Queue a -> [a] --O(n) siendo n la cantidad de elementos de la cola. Por cada elemento de la cola se hacen operaciones de orden 1 en el peor caso 
--isEmptyQ O(1) 
--dequeue O(1) 
--O(1) + O(1)
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList q = if (isEmptyQ q) --O(1)
                then []
                else firstQ q : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a --O(n²) siendo n la cantidad de elementos de la cola. En el peor caso hace un llamado a enqueue de O(n) siendo n la cantidad de elementos de la cola
--el peor caso 
--isEmptyQ O(1) 
--dequeue O(1)
--enqueue 
--O(1) + O(1) + O(n) = O(n)
--Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = if (isEmptyQ q2)
                then q1
                else unionQ (enqueue (firstQ q2) q1) (dequeue q2)




{-
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
-}

apilar :: [a] -> Stack a -- O(n) siendo n la longitud de la lista de a, por cada elemento de la lista de a hace una operación constante O(1) haciendo un llamado a push en la recursión
--PROP: Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar []     = emptySt
apilar (x:xs) = push x (apilar xs)


--desapilar :: Stack a -> [a] -- O(n) siendo n el tamaño de la pila. En el peor caso hace una operación constante por cada elemento de la pila haciendo un llamado a top O(1) y pop O(1) en la recursión
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar st = if (isEmptyS st)
                then []
                else top st : (desapilar (pop st))


--insertarEnPos :: Int -> a -> Stack a -> Stack a
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).


insertarEnPos :: Int -> a -> Stack a -> Stack a -- O(n) siendo n el tamaño de la pila en el peor caso en la posición última. Haciendo un llamado a push O(1), a top O(1) y pop O(1) en la recursión.
insertarEnPos 0 x st = push x st
insertarEnPos n x st = if (n > lenS st)
                    then error "Posicion inválida adentro de la pila"
                    else push (top st) (insertarEnPos (n-1) x (pop st))