{-
Existe un tipo abstracto llamado Mago, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Un mago puede
aprende hechizos, e informarnos su nombre y qué hechizos conoce.

El tipo Hechizo es sinónimo de String, y se corresponde con el nombre de un hechizo.

El tipo Nombre es sinónimo de String, y se corresponde con el nombre de un mago.

Podemos suponer que dos magos son iguales si poseen el mismo nombre, y un mago es más poderoso que otro si conoce más
hechizos (lo que permite ordenarlos por la cantidad de hechizos que saben).
En la escuela no existen dos magos con el mismo nombre.
-}

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
    {-
    INV.REPR: En (EDM sh mnm pqm) se cumple que:
            *Para cada nombre n en mnm, es idéntico al nombre del Mago m asociado y además m pertenece a pqm
            *En pqm, cada Mago se encuentra en mnm
            *En pqm, la cantidad de Magos es coincidente con la cantidad de Magos en mnm
            *En pqm, no hay magos repetidos
            *En pqm, los magos están ordenados de mayor a menor, correspondiente a la cantidad de hechizos que posee
            *Para todo mago m en mnm, los hechizos que conoce pertenecen a sh
            *sh representa el conjunto de todos los hechizos enseñados por la escuela
    -}

--M la cantidad de magos 
--H la cantidad de hechizos

fundarEscuela :: EscuelaDeMagia -- O(1)
--Propósito: Devuelve una escuela vacía.
fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia :: EscuelaDeMagia -> Bool -- O(1)
--Propósito: Indica si la escuela está vacía.
estaVacia (EDM sh mnm pqm) = isEmptyPQ pqm


registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia -- costo de la operación = O(log M)
--crearM = O(1)
--lookupM = O(log M) donde M es la cantidad de magos en el map
--assocM = O(log M) donde M es la cantidad de magos en el map
--insertPQ = O(log M) donde M es la cantidad de magos en la priority queue
--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
registrar n (EDM sh mnm pqm) = case (lookupM n mnm) of
                                Nothing -> let m = crearM n in 
                                    EDM sh (assocM n m mnm) (insertPQ m pqm)
                                Just m  -> EDM sh mnm pqm


magos :: EscuelaDeMagia -> [Nombre] -- costo de la operación = O(M)
--domM = O(M) donde M es la cantidad de magos en el map
--Propósito: Devuelve los nombres de los magos registrados en la escuela.
magos (EDM sh mnm pqm) = domM mnm


hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo -- costo de la operación = O(log M)
--lookupM = O(log M) donde M es la cantidad de magos en el map
--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
hechizosDe n (EDM sh mnm pqm) = case (lookupM n mnm) of
                                Nothing -> error "No existe el mago dado"
                                Just m  -> hechizos m


leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int -- costo de la operación = O(log M)
--lookupM = O(log M) donde M es la cantidad de magos del map
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
leFaltanAprender n (EDM sh mnm pqm) = case (lookupM n mnm) of
                                    Nothing -> error "No existe el mago dado"
                                    Just m  -> sizeS sh - sizeS (hechizos m)


egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia) -- costo final de la operación = O(log M)
--isEmptyPQ = O(1)
--maxPQ = O(1)
--deleteM = O (log M) donde M es la cantidad de magos del map
--deleteMaxPQ = O(log M) donde M es la cantidad de magos de la pq
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
egresarUno (EDM sh mnm pqm) = if isEmptyPQ pqm
                            then error "En la escuela no hay magos"
                            else let m = maxPQ pqm
                                mnm' = deleteM (nombre m) mnm
                                pqm' = deleteMaxPQ pqm in
                                    (m, EDM sh mnm' pqm')


enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia -- costo final de la operación = O(log H + M log M)
--lookupM = O(log M) donde M es la cantidad de Magos en el map
--aprender = O(log H) donde H es la cantidad de hechizos del mago
--addS = O(log H) donde H es la cantidad de hechizos en el Set
--assocM = O(log M) = donde M es la cantidad de magos del map
--actualizarM = O(M log M)
--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
enseñar h n (EDM sh mnm pqm) = case (lookupM n mnm) of
                                Nothing -> error "El mago no existe"
                                Just m  -> let m' = aprender h m in
                                    EDM (addS h sh) (assocM n m' mnm) (actualizarM h m' pqm)

actualizarM :: Mago -> PriorityQueue Mago -> PriorityQueue Mago --En peor caso, por cada Mago en la pq hace dos operaciones O(log M) y al encontrarlo, hace una operación O(log H) y O(log M) -> costo de la operación = O(M log M)
--isEmptyPQ = O(1)
--nombre = O(1)
--maxPQ = O(1)
--insertPQ = O(log M) donde M es la cantidad de Magos en la pq
--deleteMaxPQ = O(log M) donde M es la cantidad de Magos en la pq
actualizarM m pqm = if isEmptyPQ pqm
                        then error "El mago no existe"
                        else if (nombre m == nombre (maxPQ pqm))
                            then insertPQ m (deleteMaxPQ pqm)
                            else insertPQ (maxPQ pqm) (actualizarM h m (deleteMaxPQ pqm))

{-
Usuario
Implementar las siguientes funciones como usuario del tipo EscuelaDeMagia:
j) hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
Propósito: Retorna todos los hechizos aprendidos por los magos.

k) hayUnExperto :: EscuelaDeMagia -> Bool
Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.

l) egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
magos.

-}

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo -- costo de la operación = O(M * (H log H + log M) + M) = O(M * (H log H + log M))
--magos = O(M) donde M es la cantidad de todos los magos de la escuela de magia
--hechizosDeEscuela = O(M * (H log H + log M))
hechizosAprendidos edm = hechizosDeEscuela (magos edm) edm

hechizosDeEscuela :: [Nombre] -> EscuelaDeMagia -> Set Hechizo -- por cada N hago = O(M * (H log H + log M))
--hechizosDe = O(log M) donde M es la cantidad de magos en el map
--unionS = O(H log H) donde por cada H hace una operación O(log H) siendo H la cantidad de hechizos
hechizosDeEscuela [] edm     = emptyS
hechizosDeEscuela (n:ns) edm = unionS (hechizosDe n edm) (hechizosDeEscuela ns edm)


hayUnExperto :: EscuelaDeMagia -> Bool --costo de la operación O(log M)
--leFaltanAprender = O(log M) donde M es la cantidad de magos en la escuela de magia
--nombre = O(1)
hayUnExperto edm = let (m, _) = egresarUno edm in
    leFaltanAprender (nombre m) edm == 0


egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia) --Por cada mago M experto de la escuela hace dos operaciones O(log M) -> costo de la operación = O(M * (log M)) -> O(M log M)
--estaVacia = O(1)
--hayUnExperto = O(log M) donde M es la cantidad de magos de la escuela de magia
--egresarUno = O(log M)  donde M es la cantidad de magos de la escuela de magia
-- 
egresarExpertos edm = if estaVacia edm
                    then ([], edm)
                    else if hayUnExperto edm
                        then let (m, edm') = egresarUno edm
                                (ms, edm'') = egresarExpertos edm'
                            in (m:ms, edm'')
                        else ([], edm)