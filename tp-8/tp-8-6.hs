data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

{-Definir las siguientes funciones utilizando recursión estructural explícita e indicar el costo obtenido para cada solución
dada:
a) conLaSumaCalculada :: Tree Int -> Tree Int
Propósito: dado un árbol binario computa un árbol con misma estructura, pero en el que cada nodo posee la suma de los
elementos de todo su subárbol (incluyendo al elemento en su raíz).
Precondición: no tiene.-}

conLaSumaCalculada :: Tree Int -> Tree Int
conLaSumaCalculada t = let (t', n) = conLaSumaCalculadaT t in t'

conLaSumaCalculadaT :: Tree Int -> (Tree Int, Int) --O(N) donde N es la cantidad de nodos de todo el árbol
conLaSumaCalculadaT EmptyT          = (EmptyT, 0)
conLaSumaCalculadaT (NodeT n ti td) =   let (ti', ni') = conLaSumaCalculadaT ti
                                            (td', nd') = conLaSumaCalculadaT td
                                            n' = n+ni'+nd' in
                                                (NodeT n' ti' td', n')

caminoQueSumaMas :: Tree Int -> [Int] -- O(N)
--Propósito: dado un árbol devuelve los elementos del camino cuya suma es mayor al resto de los caminos del árbol. Los caminos van desde la raíz hacia las hojas.
--Precondición: no tiene.
caminoQueSumaMas t = let (ns, n) = caminoQueSumaMasT t in ns

caminoQueSumaMasT :: Tree Int -> ([Int], Int) --O(N) donde N es la cantidad total de nodos de todo el árbol
caminoQueSumaMasT EmptyT          = ([], 0)
caminoQueSumaMasT (NodeT n ti td) = let (nsi, ni) = caminoQueSumaMasT ti
                                        (nsd, nd) = caminoQueSumaMasT td in
                                            if (n+ni) > (n+nd)
                                            then (n:nsi, n+ni)
                                            else (n:nsd, n+nd)


--Ejercicio 2

{-
De esta representación sabemos que
el primer Map relaciona razas de aliens con aliens ordenados en base a la cantidad de habilidades que poseen;
el segundo Map relaciona habilidades con las razas que poseen dicha habilidad.
-}

data Galaxia = ConsG (Map String (PriorityQueue Alien)) (Map String [String])
                        --raza                     --habilidades     --razas
        {-
        INV REPR: En (ConsG mspqa mss) se cumple que:
                *En mspqa, cada raza r asociado a una priorityqueue alien pqa, cada Alien a en pqa es de raza r
                *En mspqa, cada Alien a de la priorityqueue Alien pqa, sus habilidades están como clave en mss. Cada clave habilidad h en mss, se encuentran como habilidad en los aliens de pqa
                *En mss, para cada habilidad h asociado a una lista de razas rs. Cada raza r en rs, se encuentra como clave en mspqa. Cada raza r en mspqa, se encuentra en rs de mss
                *En mspqa, la priorityqueue Alien pqa está ordenada de mayor a menor correspondiente a la cantidad de habilidades de los aliens
        -}
        {-
        INV REPR: En (ConsG mspqa mhr) se cumple que:
                *Para cada raza r clave en mspqa, todos los aliens de la cola asociada son de raza r.
                *Para cada habilidad h clave en mhr, cada raza r de la lista asociada aparece como clave en mspqa.
                *Para cada raza r clave en mspqa, si algún alien de esa raza posee una habilidad h,
                    entonces r aparece en la lista asociada a h en mhr.
                *Para cada habilidad h clave en mhr y cada raza r de su lista,
                    existe al menos un alien de raza r que posee la habilidad h.
        -}


{-
Definir las siguientes funciones como implementador del TAD Galaxia, indicando el costo obtenido:
a) elMasHabilidosoEntre :: [String] -> Galaxia -> [Alien]
Propósito: dada una lista de razas devuelve al alien más habilidoso entre dichas razas.
Precondición: existe al menos un alien para cada raza dada.
b) enseñarARaza :: String -> String -> Galaxia -> Galaxia
Propósito: dada una habilidad y una raza enseña dicha habilidad a todos los alien de dicha raza.
Precondición: la raza existe, pero los alien de esa raza aún no tienen dicha habilidad.
Nota: la habilidad puede o no ya existir.
-}

elMasHabilidosoEntre :: [String] -> Galaxia -> [Alien] --O(N*(log R)) por cada raza N de la lista hago una operación O(1) y logaritmica O(log R) -> costo final de la operación = O(N log R)
--lookupM = O(log R) donde R es la cantidad de razas en el map
--isEmptyPQ = O(1)
--maxPQ = O(1)
--Propósito: dada una lista de razas devuelve al alien más habilidoso entre dichas razas.
--Precondición: existe al menos un alien para cada raza dada.
elMasHabilidosoEntre [] g     = []
elMasHabilidosoEntre (r:rs) g = let (ConsG mspqa mss) = g in 
    case (lookupM r mspqa) of
    Nothing  -> error "No existe la raza"
    Just pqa -> if isEmptyPQ pqa
                then error "No existe un alien de esa raza"
                else maxPQ pqa : elMasHabilidosoEntre rs g


data Galaxia = ConsG (Map String (PriorityQueue Alien)) (Map String [String])
                        --raza                     --habilidades     --razas

enseñarARaza :: String -> String -> Galaxia -> Galaxia --O(A log A + log H + log R + N)
--lookupM = O(log R) donde R es la cantidad de razas del map
--lookupM = O(log H) donde H es la cantidad de habilidades del map
--elem = O(N) donde N es la cantidad de todas las razas que contienen esa habilidad
--assocM = O(log R) donde R es la cantidad de razas del map
--enseñarH = O(A log A)
--assocM = O(log H) donde H es la cantidad de habilidades del map
enseñarARaza h r g = let (ConsG mspqa mss) = g in
    case (lookupM r mspqa) of
        Nothing  -> error "La raza no existe"
        Just pqa -> case (lookupM h mss) of
                    Nothing -> ConsG (assocM r (enseñarH h pqa) mspqa) (assocM h [r] mss)
                    Just rs -> if (elem r rs)
                                then error "La habilidad ya fue enseñada"
                                else ConsG (assocM r (enseñarH h pqa) mspqa) (assocM h (r:rs) mss)

enseñarH :: String -> PriorityQueue Alien -> PriorityQueue Alien --O(A*(log A)) por cada alien en el peor caso hace dos operaciones logaritmicas -> costo de la operación = O(A log A)
--isEmptyPQ = O(1)
--emptyPQ = O(1)
--maxPQ = O(1)
--aprender = O(1)
--insertPQ = O(log A) donde A es la cantidad de aliens de la priorityqueue
--deleteMaxPQ = O(log A) donde A es la cantidad de aliens de la priorityqueue
enseñarH h pqa = if isEmptyPQ pqa
                then emptyPQ
                else let a = maxPQ pqa in
                    insertPQ (aprender h a) (enseñarH h (deleteMaxPQ pqa)) 