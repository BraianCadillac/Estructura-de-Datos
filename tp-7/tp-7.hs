{-
Ejercicio 1
Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que
el usuario utiliza una priority queue con costos logarítmicos de inserción y borrado (o sea, usa una
Heap como tipo de representación).
-}

{-emptyPQ :: PriorityQueue a --O(1)
Propósito: devuelve una priority queue vacía.

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
Propósito: indica si la priority queue está vacía.

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(log N)
Propósito: inserta un elemento en la priority queue.

findMinPQ :: Ord a => PriorityQueue a -> a --O(1)
Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
Precondición: parcial en caso de priority queue vacía.

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(log N)
Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
Precondición: parcial en caso de priority queue vacía.-}

-- insertar en una pq cada elemento de la lista que tiene tamaño N cuesta O (log N) y por cada N la pq va creciendo entonces, por la inserción valdría O(N log N). Luego con una función auxiliar haríamos una operación constante que es findmind pq de costo O(1) y deleteMin pq que cuesta O(log N) siendo N la cantidad de elementos de la pq sin el primer elemento, quedaría O(N log N) haciendo una operación O(log N) N veces. Entonces el costo total para la función de heapsort -> O(N log N + N log N) = O(N log N)


{-
Ejercicio 2
Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
desbalancearse al insertar o borrar elementos). En todos los costos, 
-N es la cantidad de elementos del árbol. 
Justificar por qué la implementación satisface los costos dados.
-}

{-
INV REPR: El tree cumple con ser un BST
-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

belongsBST :: Ord a => a -> Tree a -> Bool -- O(log n) siendo n la cantidad de nodos del árbol en promedio. En el peor caso sería O(n) si el árbol estuviera desbalanceado
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST x EmptyT          = False
belongsBST x (NodeT y ti td) = if x == y then True
                                else if x < y
                                    then belongsBST x ti
                                    else belongsBST x td

insertBST :: Ord a => a -> Tree a -> Tree a --O(log n) siendo n la cantidad de nodos del árbol. En el peor caso si el árbol estuviera desbalanceado sería lineal O(n)
--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N)
insertBST x EmptyT          = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if (x == y) 
                            then NodeT x ti td
                            else if (x < y)
                                then NodeT y (insertBST x ti) td
                                else NodeT y ti (insertBST x td)

deleteBST :: Ord a => a -> Tree a -> Tree a --O(log n) siendo n la cantidad de elementos del árbol
--Propósito: dado un BST borra un elemento en el árbol.
--Costo: O(log N)
deleteBST x EmptyT          = EmptyT
deleteBST x (NodeT y ti td) =
        if (x == y)
        then (reArmarBST ti td)
        else if x < y
            then NodeT y (deleteBST x ti) td
            else NodeT y ti (deleteBST x td)

reArmarBST :: Ord a => Tree a -> Tree a -> Tree a
--PRECOND: Ambos son BST. Ambos no están vacíos
reArmarBST EmptyT td = td
reArmarBST ti EmptyT = ti
reArmarBST ti td     = NodeT (maxBST ti) (sinElMaxBST ti) td

maxBST :: Ord a => Tree a -> a
--PRECOND: Es BST, no está vacío
maxBST (NodeT x _ EmptyT) = x
maxBST (NodeT _ _ td)     = maxBST td

sinElMaxBST :: Ord a => Tree a -> Tree a
--PRECOND: Es BST, no está vacío
sinElMaxBST (NodeT x ti EmptyT) = ti 
sinElMaxBST (NodeT x ti td)      = NodeT x ti (sinElMaxBST td)

splitMinBST :: Ord a => Tree a -> (a, Tree a) --O(log N) siendo n la cantidad de elementos del árbol
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)
--PRECOND: El árbol no está vacío
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST (NodeT x ti td) =
  let (m, ti') = splitMinBST ti
  in (m, NodeT x ti' td)




splitMaxBST :: Ord a => Tree a -> (a, Tree a) --O(log n) siendo n la cantidad de elementos en el árbol
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--Costo: O(log N)
--PRECOND: El árbol no está vacío
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST (NodeT x ti td)     = 
    let (m, td') = splitMaxBST td in
        (m, NodeT x ti td')


esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x ti td) = esBST ti && esBST td &&
        (case ti of
            EmptyT -> True
            _      -> maxBST ti < x) &&
        (case td of
            EmptyT -> True
            _      -> minBST td > x)


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
--Costo: O(log N)
elMaximoMenorA x EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) = if (y < x)
                                    then case (elMaximoMenorA x td) of
                                        Nothing -> Just y
                                        Just z  -> Just z
                                    else elMaximoMenorA x ti

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
--Costo: O(log N)
elMinimoMayorA x EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) = if (y > x)
                                    then case (elMinimoMayorA x td) of
                                        Nothing -> Just y
                                        Just z  -> Just z
                                    else elMinimoMayorA x ti


balanceado :: Tree a -> Bool
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
--Costo: O(N2)
balanceado EmptyT = True
balanceado (NodeT _ ti td) = balanceado ti && balanceado td && abs (heightT ti - heightT td) <= 1

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ ti td) = 1 + max (heightT ti) (heightT td)



---------------------------------------------------------------------------------

-- Donde se observa que:
-- los empleados son un tipo abstracto.
-- el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
-- el segundo map relaciona empleados con su número de CUIL.
-- un empleado puede estar asignado a más de un sector
-- tanto Map como Set exponen una interfaz eficiente con costos logarítmicos para inserción,
-- búsqueda y borrado, tal cual vimos en clase.





type SectorId = Int
type CUIL = Int

data Empresa = ConsE  (Map SectorId (Set Empleado))
                    (Map CUIL Empleado)
    {-
        INV REPR:
            En (ConsE msse mce) se cumple que:

                * Todo empleado que aparece en algún Set de msse, aparece como valor en mce.
                * Todo empleado que aparece como valor en mce, pertenece a algún Set de msse.
                * Para cada par clave-valor en mce, la clave coincide con el CUIL del empleado asociado.
    -}

consEmpresa :: Empresa
--Propósito: construye una empresa vacía.
--Costo: O(1)
consEmpresa = ConsE emptyM emptyM

buscarPorCUIL :: CUIL -> Empresa -> Empleado -- O(log E) siendo E la cantidad de empleados en el map, en el peor caso puede estar al final
--Propósito: devuelve el empleado con dicho CUIL.
--Precondición: el CUIL es de un empleado de la empresa.
--Costo: O(log E)
buscarPorCUIL c (ConsE msse mce) = case (lookupM c mce) of
                                    Nothing -> error "No existe el cuil dado"
                                    Just e  -> e

empleadosDelSector :: SectorId -> Empresa -> [Empleado] -- O(log S + E) siendo S la cantidad de Sectores en el map y E todos los empleados del set
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(log S + E)
empleadosDelSector sd (ConsE msse mce) = case (lookupM sd msse) of
                                        Nothing -> error "No existe el sector Id"
                                        Just se -> setToList se 

todosLosCUIL :: Empresa -> [CUIL] -- O(E) siendo E todos los cuils de los Empleados de la empresa
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E)
todosLosCUIL (ConsE msse mce) = keys mce

todosLosSectores :: Empresa -> [SectorId] --O(S) siendo S todos los Sectores de la empresa
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S)
todosLosSectores (ConsE msse mce) = keys msse


agregarSector :: SectorId -> Empresa -> Empresa --O(log S) siendo S la cantidad de sectores de msse, en caso de no estar lo agrega -> assocM que cuesta O(log S) = O(log S + log S) = O(log S)
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(log S)
agregarSector sd (ConsE msse mce) = case (lookupM sd msse) of
                                    Nothing -> ConsE (assocM sd emptyS msse) mce
                                    Just se -> error "Ya existe el sector Id"

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa -- O(K (log S + E))
--Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
--CUIL dado.
--Costo: calcular.
agregarEmpleado sids c (ConsE msse mce) = 
    let e = consEmpleado c in
        ConsE (insertarSectoresA sids e msse) (insertarEmpleadoA c e mce)

insertarEmpleadoA :: CUIL -> Empleado -> Map CUIL Empleado -> Map CUIL Empleado --O(log E) siendo E la cantidad de cuils de Empleados que hay en el map, en caso de no estar el CUIL dado, lo agrega haciendo un llamado a assocM -> O(log E + log E) = O (log E)
insertarEmpleadoA c e mce = case (lookupM c mce) of
                            Nothing -> assocM c e mce
                            Just e  -> error "El CUIL dado ya existe en la Empresa"

insertarSectoresA :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) -- K*(log S + log S) -> por cada sectorId (K) hace un llamado a lookupM de costo O (log S) que en el peor caso puede estar al final + hace un llamado a assocM para agregar al Empleado -> COSTO FINAL = O(K (log S + E)), siendo E la cantidad de Empleados del set es decir por cada sector Id hace una operación log S 
insertarSectoresA [] e msse       = msse
insertarSectoresA (si:sis) e msse = case (lookupM si msse) of
                                    Nothing -> assocM si emptyS (insertarSectoresA sis e msse) 
                                    Just se -> assocM si (addS e se) (insertarSectoresA sis e msse) 


agregarASector :: SectorId -> CUIL -> Empresa -> Empresa --Hace un llamado a lookupM de costo O(log E) siendo E la cantidad de empleados del map, en el peor caso puede estar al final y hace un llamado a assocM de costo O(log E), incorporarSector de costo O (log S) siendo S la cantidad de sectores del empleado = Haciendo una suma de los costos, quedaría : O (log S + E)
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular.
agregarASector si c (consE msse mce) = case (lookupM c mce) of
                                        Nothing -> error "No existe el empleado con el CUIL dado"
                                        Just e  -> ConsE (agregarSector si e msse) (assocM c (incorporarSector si e) mce)


agregarSector :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) --O(log S) siendo S la cantidad de sectorId del map, en el peor caso puede estar al final y hace un llamado a assocM de costo O(log S), y hace un llamado a addS de costo O(E) siendo E la cantidad de Empleados del set. Costo final = O(log S + log S + E) = O(log S + E)
agregarSector si e msse = case (lookupM si msse) of
                            Nothing -> error "No existe el sectorId dado"
                            Just se -> assocM si (addS e se) msse 



borrarEmpleado :: CUIL -> Empresa -> Empresa --Hace un llamado a lookupM de costo O(log E) siendo E la cantidad de Empleados del map, deleteM siendo el mismo O(log E). Hace un llamado a borrarEmpleadoEn y keys de costo O(S) siendo S la cantidad de SectoresId de todo el map. Costo total por borrarEmpleadoEn -> O(S (log S + E))
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: calcular.
borrarEmpleado c (ConsE msse mce) = case (lookupM c mce) of
                                    Nothing -> error "El empleado con el CUIL dado no existe"
                                    Just e  -> ConsE (borrarEmpleadoEn (keys msse) e msse) (deleteM c mce)

borrarEmpleadoEn :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) -- (S*(log S + log S + E + E) Por cada sectorId es decir S, hace una operación log S que en peor caso puede estar al final, luego hace un llamado a assocM de costo O (log S), belongs de costo O(E) siendo E la cantidad de Empleados del set y removeS de costo O(E) siendo E la cantidad de Empleados del set, el mismo E -> costo con las sumas de operaciones -> O(S (log S + E))
borrarEmpleadoEn [] e msse       = msse
borrarEmpleadoEn (sd:sds) e msse = case (lookupM sd msse) of
                                    Nothing -> error "El sectorId no existe"
                                    Just se -> if (belongs e se)
                                                then assocM sd (removeS e se) (borrarEmpleadoEn sds e msse)
                                                else assocM sd se (borrarEmpleadoEn sds e msse)


--Y sabemos que la interfaz de Empleado es:

--consEmpleado :: CUIL -> Empleado
--Propósito: construye un empleado con dicho CUIL.
--Costo: O(1)

--cuil :: Empleado -> CUIL
--Propósito: indica el CUIL de un empleado.
--Costo: O(1)

--incorporarSector :: SectorId -> Empleado -> Empleado
--Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
--Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

--sectores :: Empleado -> [SectorId]
--Propósito: indica los sectores en los que el empleado trabaja.
--Costo: O(S)


{-
Ejercicio 5
Como usuario del tipo Empresa implementar las siguientes operaciones, calculando el costo obtenido
al implementarlas, y justificando cada uno adecuadamente.

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
Propósito: construye una empresa con la información de empleados dada. Los sectores no
tienen empleados.
Costo: calcular.

recorteDePersonal :: Empresa -> Empresa
Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
Costo: calcular.

convertirEnComodin :: CUIL -> Empresa -> Empresa
Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
Costo: calcular.

esComodin :: CUIL -> Empresa -> Bool
Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
Costo: calcular.
-}

comenzarCon :: [SectorId] -> [CUIL] -> Empresa -- O(K log K + C log C) por cada K sectorid, hace una operación log S siendo s la cantidad de sectores id de la empresa, va creciendo por cada K. Por cada C cuil, hace una operación en el peor caso (log S) en el caso le mando una lista vacía
comenzarCon [] []       = consEmpresa
comenzarCon [] (c:cs)   = agregarEmpleado [] c (comenzarCon [] cs)
comenzarCon (si:sis) cs = agregarSector si (comenzarCon sis cs)

recorteDePersonal :: Empresa -> Empresa --COSTO FINAL = O (C (S (log S + E)))
{- 
O(C) = todosLosCUIL
O(C) = lenght de todos los cuil
empleadosASacar = O(C) con respecto al número entero de empleados a sacar
borrarN = O (C (S (log S + E)))
-}
recorteDePersonal e = 
    let te = (todosLosCUIL e)
    n = div (length (todosLosCUIL e)) 2 
    es = empleadosASacar te n in
        borrarN es e 

empleadosASacar :: [CUIL] -> Int -> [CUIL] -- O(C) siendo C el número entero = a la cantidad de empleados a sacar 
--PRECOND : n < length cs
empleadosASacar _ 0      = [] 
empleadosASacar (c:cs) n = c : empleadosASacar cs (n-1)

borrarN :: [CUIL] -> Empresa -> Empresa -- O (C (S (log S + E))) por cada CUIL c hace un llamado a borrarEmpleado de costo O(S (log S + E)) 
borrarN []     e = e
borrarN (c:cs) e = borrarEmpleado c (borrarN cs e)


convertirEnComodin :: CUIL -> Empresa -> Empresa -- O (S (log S + E)) + S 
--convertirEnComodin O (S (log S + E))
--todosLosSectores O(S)
convertirEnComodin c e = convertirEnComodinEn c (todosLosSectores e) e

convertirEnComodinEn :: CUIL -> [SectorId] -> Empresa -> Empresa -- O (S (log S + E)) por cada sectorId S hace una operación (log S + E) siendo S todos los sectores de la empresa
--PRECOND: sis son sectores válidos adentro de la empresa
convertirEnComodinEn c [] e       =  e
convertirEnComodinEn c (si:sis) e = agregarASector si c (convertirEnComodinEn c sis e)

esComodin :: CUIL -> Empresa -> Bool -- por esComodinEn O(S (log S + E)) -> costo final = O(S (log S + E) + log E)
esComodin c e = let sis = todosLosSectores e 
                e' = buscarPorCUIL c e in
                    esComodinEn e' sis e
--buscarPorCUIL O(log E) siendo E la cantidad de empleados de la empresa
--todosLosSectores O(S) siendo S todos los sectores de la empresa

esComodinEn :: Empleado -> [SectorId] -> Empresa -> Bool 
-- O(S (log S + E) ) -> por cada sectorId S hago una operación cuadrática por elem preguntando si se encuentra en la lista de empleados de cada sector de la empresa de costo por empleadosDelSector (log S + E)
esComodinEn em [] e       = True
esComodinEn em (si:sis) e = elem em (empleadosDelSector si e) && esComodinEn em sis e 


{-
consEmpresa :: Empresa
Propósito: construye una empresa vacía.
Costo: O(1)

buscarPorCUIL :: CUIL -> Empresa -> Empleado
Propósito: devuelve el empleado con dicho CUIL.
Precondición: el CUIL es de un empleado de la empresa.
Costo: O(log E)

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
Propósito: indica los empleados que trabajan en un sector dado.
Costo: O(log S + E)

todosLosCUIL :: Empresa -> [CUIL]
Propósito: indica todos los CUIL de empleados de la empresa.
Costo: O(E)

todosLosSectores :: Empresa -> [SectorId]
Propósito: indica todos los sectores de la empresa.
Costo: O(S)

agregarSector :: SectorId -> Empresa -> Empresa
Propósito: agrega un sector a la empresa, inicialmente sin empleados.
Costo: O(log S)

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá elCUIL dado.
Costo: calcular.

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
Propósito: agrega un sector al empleado con dicho CUIL.
Costo: calcular.

borrarEmpleado :: CUIL -> Empresa -> Empresa
Propósito: elimina al empleado que posee dicho CUIL.
Costo: calcular.
-}