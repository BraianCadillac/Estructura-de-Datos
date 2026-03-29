


data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show


{-
nroBolitas :: Color -> Celda -> Int
Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
existe una operación sobre listas que ayude a resolver el problema.
-}

{-
8. apariciones :: Eq a => a -> [a] -> Int
Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
-}

celdaConBolitas = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

apariciones :: Color -> [Color] -> Int
apariciones c []     = 0
apariciones c (x:xs) = unoSi (esMismoColor c x) + apariciones c xs


nroBolitas :: Color -> Celda -> Int
nroBolitas c cl = apariciones c (bolitasDe cl)


bolitasDe :: Celda -> [Color]
bolitasDe CeldaVacia    = []
bolitasDe (Bolita c cl) = c : bolitasDe cl





{-
poner :: Color -> Celda -> Celda
Dado un color y una celda, agrega una bolita de dicho color a la celda.
-}

poner :: Color -> Celda -> Celda
poner co CeldaVacia    = Bolita co CeldaVacia
poner co (Bolita c cl) = Bolita c (poner co cl)

{-
sacar :: Color -> Celda -> Celda
Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.
-}

sacar :: Color -> Celda -> Celda
sacar co CeldaVacia    = CeldaVacia
sacar co (Bolita c cl) = if (esMismoColor co c)
                            then cl
                            else Bolita c (sacar co cl)


esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _       = False


{-
ponerN :: Int -> Color -> Celda -> Celda
Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
-}

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c cl = cl
ponerN n c cl = poner c (ponerN (n-1) c cl)


{-
1.2. Camino hacia el tesoro
Tenemos los siguientes tipos de datos
-}

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

{-
hayTesoro :: Camino -> Bool
Indica si hay un cofre con un tesoro en el camino.
-}

hayTesoro :: Camino -> Bool
hayTesoro Fin          = False
hayTesoro (Cofre os c) = hayTesoroEn' os || hayTesoro c
hayTesoro (Nada c)     = hayTesoro c

hayTesoroEn' :: [Objeto] -> Bool
hayTesoroEn' []     = False
hayTesoroEn' (o:os) = esTesoro o || hayTesoroEn' os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

{-
pasosHastaTesoro :: Camino -> Int
Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.
-}

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin          = error "No hay tesoro en el camino"
pasosHastaTesoro (Cofre os c) = if (hayTesoroEn' os)
                                then 0
                                else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c)     = 1 + pasosHastaTesoro c


{-
hayTesoroEn :: Int -> Camino -> Bool
Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.
-}

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin          = False
hayTesoroEn 0 (Cofre os c) = hayTesoroEn' os
hayTesoroEn n (Cofre os c) = hayTesoroEn (n-1) c
hayTesoroEn 0 (Nada c)     = False
hayTesoroEn n (Nada c)     = hayTesoroEn (n-1) c

{-
alMenosNTesoros :: Int -> Camino -> Bool
Indica si hay al menos "n" tesoros en el camino.
-}

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 Fin          = True
alMenosNTesoros n Fin          = False
alMenosNTesoros 0 (Cofre os c) = True
alMenosNTesoros n (Cofre os c) = if (hayTesoroEn' os) 
                                    then alMenosNTesoros (n-1) c
                                    else alMenosNTesoros n c
alMenosNTesoros 0 (Nada c)     = True
alMenosNTesoros n (Nada c)     = alMenosNTesoros n c


{-
(desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.
-}



cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 m c            = cantTesorosHasta m c
cantTesorosEntre _ _ Fin          = 0
cantTesorosEntre n m (Cofre os c) = cantTesorosEntre (n-1) (m-1) c
cantTesorosEntre n m (Nada c)     = cantTesorosEntre (n-1) (m-1) c
 

cantTesorosHasta :: Int -> Camino -> Int
cantTesorosHasta 0 Fin          = 0
cantTesorosHasta n Fin          = 0
cantTesorosHasta 0 (Cofre os c) = cantTesoros os 
cantTesorosHasta n (Cofre os c) = cantTesoros os + cantTesorosHasta (n-1) c
cantTesorosHasta 0 (Nada c)     = 0
cantTesorosHasta n (Nada c)     = cantTesorosHasta (n-1) c

cantTesoros :: [Objeto] -> Int
cantTesoros []     = 0
cantTesoros (o:os) = unoSi (esTesoro o) + cantTesoros os

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

{-
2. Tipos arbóreos
2.1. Árboles binarios
-}

{-
defina las siguientes funciones utilizando recursión estructural según corresponda:
1. sumarT :: Tree Int -> Int
Dado un árbol binario de enteros devuelve la suma entre sus elementos.
-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

{-
sizeT :: Tree a -> Int
Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
en inglés).
-}

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x ti td) = 1 + sizeT ti + sizeT td

{-
mapDobleT :: Tree Int -> Tree Int
Dado un árbol de enteros devuelve un árbol con el doble de cada número.
-}

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n ti td) = NodeT (n*2) (mapDobleT ti) (mapDobleT td)

{-
perteneceT :: Eq a => a -> Tree a -> Bool
Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
árbol.
-}

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT          = False
perteneceT x (NodeT y ti td) = x==y || perteneceT x ti || perteneceT x td


{-
aparicionesT :: Eq a => a -> Tree a -> Int
Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
iguales a e.
-}

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT          = 0
aparicionesT x (NodeT y ti td) = if x==y 
                                then 1 + aparicionesT x ti + aparicionesT x td
                                else aparicionesT x ti + aparicionesT x td

{-
leaves :: Tree a -> [a]
Dado un árbol devuelve los elementos que se encuentran en sus hojas.
NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
-}

leaves :: Tree a -> [a]
leaves EmptyT          = []
leaves (NodeT x ti td) = if (isEmpty ti && isEmpty td) 
                        then x : leaves ti ++ leaves td
                        else leaves ti ++ leaves td

isEmpty :: Tree a -> Bool
isEmpty EmptyT = True
isEmpty _      = False

{-
heightT :: Tree a -> Int
Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es
la cantidad de niveles del árbol1. La altura para EmptyT es 0, y para una hoja
es 1.
-}

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x ti td) = 1 + max (heightT ti) (heightT td)

{-
mirrorT :: Tree a -> Tree a
Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con
el derecho, en cada nodo del árbol.
-}

mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)

{-
toList :: Tree a -> [a]
Dado un árbol devuelve una lista que representa el resultado de recorrerlo en
modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo,
luego la raiz y luego los elementos del hijo derecho.
-}

toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x ti td) = toList ti ++ [x] ++ toList td

{-
levelN :: Int -> Tree a -> [a]
Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El
nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la
raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.
-}

levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT          = []
levelN 0 (NodeT x ti td) = [x] 
levelN n EmptyT          = []
levelN n (NodeT x ti td) = levelN (n-1) ti ++ levelN (n-1) td

{-
11. listPerLevel :: Tree a -> [[a]]
Dado un árbol devuelve una lista de listas en la que cada elemento representa
un nivel de dicho árbol.
-}

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x ti td) = [x] : juntarNiveles (listPerLevel ti)  (listPerLevel td)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss            = yss
juntarNiveles xss []            = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

{-
ramaMasLarga :: Tree a -> [a]
Devuelve los elementos de la rama más larga del árbol
-}

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = if (heightT ti > heightT td)
                                then x : (ramaMasLarga ti) 
                                else x : (ramaMasLarga td)

{-
13. todosLosCaminos :: Tree a -> [[a]]
Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz
hasta cualquiera de los nodos.
ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o
sea, de la raíz hasta la hoja), o sea, por ejemplo
todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
EmptyT)
(NodeT 4 (NodeT 5 EmptyT EmptyT)
EmptyT))
= [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
-}

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x ti td) = [ [x] ] ++
                                juntarCamino x (todosLosCaminos ti) ++
                                juntarCamino x (todosLosCaminos td)

juntarCamino :: a -> [[a]] -> [[a]]
juntarCamino x []       = []
juntarCamino x (ys:yss) = (x:ys) : juntarCamino x yss

arbol :: Tree Int 
arbol = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) (NodeT 4 (NodeT 5 EmptyT EmptyT) EmptyT)

{-
2.2. Expresiones Aritméticas
El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:
data ExpA = Valor Int
| Sum ExpA ExpA
| Prod ExpA ExpA
| Neg ExpA
-}

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

{-
eval :: ExpA -> Int
Dada una expresión aritmética devuelve el resultado evaluarla.
-}

eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e)      = - (eval e)

{-
simplificar :: ExpA -> ExpA
Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x
-}

simplificar :: ExpA -> ExpA
simplificar (Valor n)    = Valor n
simplificar (Sum e1 e2)  = simplificarSuma (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2) 
simplificar (Neg e)      = simplificarNeg (simplificar e)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) e2 = e2
simplificarSuma e1 (Valor 0) = e1
simplificarSuma e1 e2        = Sum e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd (Valor 1) e2 = e2
simplificarProd e1 (Valor 1) = e1
simplificarProd e1 e2               = Prod e1 e2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e) = e
simplificarNeg e       = Neg e