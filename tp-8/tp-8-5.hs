{-
Ejercicios
Recordatorio: De existir, agregue las precondiciones en las funciones solicitadas. ¡No deje de dividir en subtareas! Y no olvide
además incluir propósito y precondiciones de las funciones auxiliares que necesite programar.
a) Implementar las siguientes funciones como usuario del TAD Nave, establecer su eficiencia y justificarla:

a) tripulantes :: Nave -> Set Tripulante
Propósito: Denota los tripulantes de la nave

b) Opcional (Bonus): bajaDeTripulante :: Tripulante -> Nave -> Nave
Propósito: Elimina al tripulante de la nave.
Pista: Considere reconstruir la nave sin ese tripulante.
-}

tripulantes :: Nave -> Set Tripulante -- O(S*(log S + T log T) + S) -------> O(S*(log S + T log T)
--sectores = O(S) donde S son todos los sectores de la nave
tripulantes n = tripulantesDeNave (sectores n) n

tripulantesDeNave :: [Sector] -> Nave -> Set Tripulante -- O(S*(log S + T log T))
--tripulantesDe = O(log S) donde S es la cantidad de sectores de la nave
--unionS = O(T log T) = donde por cada Tripulante hace una operación O(log T) donde T es la cantidad de tripulantes del set
tripulantesDeNave [] n     = emptyS
tripulantesDeNave (s:ss) n = unionS (tripulantesDe s n) (tripulantesDeNave ss n)


bajaDeTripulante :: Tripulante -> Nave -> Nave
bajaDeTripulante t n = let ss = sectores n
                        n' = naveVacia ss
                        sts = tripulantesToSet ss n in
                            construirSin t ss sts n' 


tripulantesToSet :: [Sector] -> Nave -> [Set Tripulante]
tripulantesToSet [] n     = []
tripulantesToSet (s:ss) n = (tripulantesDe s n) : (tripulantesToSet ss n)

construirSin :: Tripulante -> [Sector] -> [Set Tripulante] -> Nave -> Nave
--PRECOND: Ambas listas tienen la misma longitud
construirSin t [] [] n           = n
construirSin t (s:ss) (st:sts) n =  construirSin t ss sts (agregarSinT t s (set2list st) n)

agregarSinT :: Tripulante -> Sector -> [Tripulante] -> Nave -> Nave 
agregarSinT t' s [] n     = n
agregarSinT t' s (t:ts) n = if t == t'
                            then agregarSinT t' s ts n
                            else agregarSinT t' s ts (agregarTripulante t s n)

--b)
{-
Cada tripulante puede estar en un sector como máximo.

Se guarda al sector con más tripulantes de la nave y cuántos tripulantes tiene ese sector.

Los tripulantes se ordenan por rango de mayor a menor en la Heap
(no se confunda, findMin devuelve al tripulante con mayor rango).
-}

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
    {-
    INV. REPR: En (MkN msst ht si) se cumple que:
            *Para cada Sector s y su Set Tripulante st asociado en msst, cada Tripulante t en st corresponde únicamente a s.
            *Para cada Set Tripulante st en msst, cada Tripulante t en st es idéntico al que se encuentra en ht. Cada Tripulante en ht, es idéntico al que se encuentra en algún único valor st en msst.
            *En si (s, n) s es el sector con más tripulantes de la nave y n es un entero donde describe la cantidad de tripulantes en ese sector.
            *En ht cada Tripulante t está ordenado de mayor a menor correspondiente a su rango
    -}

naveVacia :: [Sector] -> Nave --O(S*(log S)) por cada Sector S se hace una operación O(log S) en cada iteración donde el map va creciendo. Costo final de la operación : O(S log S)
--agregarS = O(log S) 
--assocM = O(log S) donde S es la cantidad de sectores del map
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
naveVacia []     = error "La lista de sectores está vacía"
naveVacia (s:ss) = if null ss
                    then MkN (assocM s emptyS emptyM) emptyH (s, 0)
                    else agregarS s (naveVacia ss)

agregarS :: Sector -> Nave -> Nave --O(log S)
--assocM = O(log S) donde S es la cantidad de sectores del map
agregarS s (MkN msst ht si) = MkN (assocM s emptyS msst) ht si


tripulantesDe :: Sector -> Nave -> Set Tripulante -- O(log S)
--lookupM = O(log S) donde S es la cantidad de sectores del map
--Propósito: Obtiene los tripulantes de un sector.
tripulantesDe s (MkN msst ht si) = case (lookupM s msst) of
                                    Nothing -> error "No existe el sector"
                                    Just st -> st

sectores :: Nave -> [Sector] -- O(S)
--domM = O(S) donde S es la cantidad total de sectores de la nave
--Propósito: Denota los sectores de la nave
sectores (MkN msst ht si) = domM msst

conMayorRango :: Nave -> Tripulante --O(1)
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
conMayorRango (MkN msst ht si) = findMin ht

conMasTripulantes :: Nave -> Sector -- O(1)
--Propósito: Denota el sector de la nave con más tripulantes.
conMasTripulantes (MkN msst ht si) = let (s, n) = si in s

conRango :: Rango -> Nave -> Set Tripulante -- O(P log P) por conRangoEn
--Propósito: Denota el conjunto de tripulantes con dicho rango.
conRango r (MkN msst ht si) = conRangoEn r ht 

conRangoEn :: Rango -> Heap Tripulante -> Set Tripulante --O(P*(log P + log P)) -> Por cada tripulante se hacen operaciones O(log P) donde el set del tripulante con el rango dado va creciendo. Costo de la operación : O(P log P)
--isEmptyH = O(1)
--findMin = O(1)
--addS = O(log P) donde P es la cantidad de tripulantes en el set, va creciendo por cada tripulante con el rango dado
--deleteMin = O(log P) donde P es la cantidad de tripulantes de la heap
conRangoEn r ht = if isEmptyH ht
                then emptyS
                else let t = findMin ht in
                    if (r == rango t) 
                    then addS t (conRangoEn r (deleteMin ht))
                    else conRangoEn r (deleteMin ht)


sectorDe :: Tripulante -> Nave -> Sector -- costo final de la operación = O(S*(log S + log P))
--domM = O(S) donde S es la cantidad de sectores de toda la nave
--propósito: Devuelve el sector en el que se encuentra un tripulante.
--precondición: el tripulante pertenece a la nave.
sectorDe t (MkN msst ht si) = sector t (domM msst) msst 

sector :: Tripulante -> [Sector] -> Map Sector (Set Tripulante) -> Sector --O(S*(log S + log P)) donde por cada S hago dos operaciones logaritmicas -> O(log S) + O(log P)
--lookupM = O(log S) donde S es la cantidad de sectores del map
--belongs = O(log P) donde P es la cantidad de tripulantes en el set
--PRECOND: ss son claves válidas en msst
sector t [] msst     = error "No existe el tripulante en la nave"
sector t (s:ss) msst = case (lookupM s msst) of
                        Nothing -> error "El sector del tripulante no existe"
                        Just st -> if (belongs t st)
                                    then s
                                    else sector t ss msst

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave -- costo de la operación : O(P log P + log S) -> los demás costos quedan absorvidos
--tripulantesDeLaNave = O(P log P)
--elem = O(P) en el peor caso P es la cantidad de tripulantes de la longitud de la lista
--lookupM = O(log S) donde S es la cantidad de sectores del map
--addS = O(log P) donde P es la cantidad de tripulantes del set
--assocM = O(log S) donde S es la cantidad de sectores del map
--insertH = O(log P) donde P es la cantidad de tripulantes de la heap
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
agregarTripulante t s (MkN msst ht si) = let ts = tripulantesDeLaNave ht in
                                            if (elem t ts)
                                            then error "El tripulante se encuentra en la nave"
                                            else case (lookupM s msst) of
                                                Nothing -> error "El sector no existe en la nave"
                                                Just st -> let st' = addS t st 
                                                            msst'  = assocM s st' msst
                                                            ht'    = insertH t ht in
                                                                MkN msst' ht' (nuevoS si (s, sizeS st'))

tripulantesDeLaNave :: Heap Tripulante -> [Tripulante] -- O(P*(log P)) por cada Tripulante hago una operación logaritmica O(log P) (deleteMin) -> costo de la operación = O(P log P)
--isEmptyH = O(1)
--findMin = O(1)
--deleteMin = O(log P) donde P es la cantidad de tripulantes de la heap
tripulantesDeLaNave ht = if isEmptyH ht
                        then []
                        else findMin ht : tripulantesDeLaNave (deleteMin ht)



nuevoS :: (Sector, Int) -> (Sector, Int) -> (Sector, Int) -- O(1)
nuevoS sn1 sn2 = let (s, n) = sn1
                (s',n')     = sn2 in
                    if n>n'
                    then sn1
                    else sn2