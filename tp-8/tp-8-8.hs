{-
El objetivo de esta evaluación es modelar un torneo de fútbol con el registro de equipos y los goles de sus jugadores. Para ello,
definiremos un tipo abstracto llamado Torneo, donde damos por hecho que:

Existe un tipo abstracto llamado Equipo, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Un equipo
posee un nombre, los jugadores que juegan en dicho equipo y los goles anotados.
El tipo Nombre es sinónimo de String, y se corresponde con el nombre de un jugador o de un equipo.
Podemos suponer que dos jugadores o equipos son iguales si poseen el mismo nombre, y un jugador o equipo es más goleador
que otro si posee más goles realizados (lo que permite ordenarlos por la cantidad de goles).

-Un jugador sólo puede pertenecer a un equipo.
-No existen dos jugadores con el mismo nombre en el torneo.
-No existen dos equipos con el mismo nombre en el torneo.
Representación
Dicho esto, la representación que utilizaremos será la siguiente (que no es posible modificar): 
-}

data Torneo = ConsT (Map Nombre Equipo)
            -- nombreDeEquipo 
                    (Map Nombre Nombre)
            --nombreDeJugadores -nombreDeEquipo 
                    (PriorityQueue Equipo)
        {-
        INV REPR: En (ConsT mne mnn pqe) se cumple que:
                *En mne, cada Nombre n asociado a un Equipo e, el nombre de equipo de e es idéntico a n
                *En mne, cada valor Equipo e, es idéntico al que se encuentra en pqe. Cada Equipo en pqe, es idéntico a e en mne
                *En mnn, cada valor Nombre ne se encuentra como clave en mne
                *En mnn, cada Nombre nj asociado a un Nombre ne, el jugador nj pertenece únicamente al equipo ne
                *En pqe, cada Equipo e está ordenado de mayor a menor correspondiente a la cantidad de goles convertidos
        -}

{-
Esta representación utiliza:
Un Map que relaciona a los equipos con su nombre.
Un Map que relaciona nombres de jugadores (las claves) con nombres de equipos (los valores).
Una PriorityQueue que posee a todos los equipos del torneo, y que permite obtenerlos de forma eficiente de mayor a menor
en base a la cantidad de goles que acertaron.
-}

equipo :: Nombre -> Torneo -> Maybe Equipo -- O(log E)
--lookupM = O(log E) donde E es la cantidad de equipos en el map
--Propósito: dado un nombre de equipo devuelve al equipo con dicho nombre.
equipo ne (ConsT mne mnn pqe) = lookupM ne mne

equipoDe :: Nombre -> Torneo -> Maybe Equipo --O(log J + log E)
--lookupM = O(log J) donde J es la cantidad de jugadores del map
--lookupM = O(log E) donde E es la cantidad de equipos del map
--Propósito: dado un nombre de jugador devuelve el equipo en el que juega.
--Nota: el jugador puede no existir, pero si existe, su equipo también.
equipoDe nj (ConsT mne mnn pqe) = case (lookupM nj mnn) of
                                Nothing -> Nothing
                                Just ne -> lookupM ne mne

jugadores :: Torneo -> [Nombre]
--O(J) donde J son todos los jugadores del torneo
--Propósito: denota la lista de jugadores del torneo
jugadores (ConsT mne mnn pqe) = domM mnn

equipoGoleador :: Torneo -> Equipo --O(1)
--isEmptyPQ = O(1)
--maxPQ = O(1)
--Propósito: indica el equipo que más goles anotó.
--Precondición: existe al menos un equipo en el torneo.
equipoGoleador (ConsT mne mnn pqe) = if isEmptyPQ pqe
                                    then error "No existe equipo en el torneo"
                                    else maxPQ pqe



comenzarTorneo :: [Equipo] -> Torneo --O(E*(log E + J log J))
--nombre = O(1)
--assocM = O(log E) donde E es la cantidad de equipos del map
--asociarJ = O(J log J)
--jugadores = O(J) donde J es la cantidad total de jugadores del equipo
--insertPQ = O(log E) donde E es la cantidad de equipos de la priority Queue
--Propósito: devuelve un torneo en el que participan los equipos dados.
--Nota: los equipos ya poseen jugadores, no olvidar sumarlos a la estructura.
comenzarTorneo []     = ConsT emptyM emptyM emptyPQ
comenzarTorneo (e:es) = case (comenzarTorneo es) of
                        (ConsT mne mnn pqe) -> let n = nombre e in
                            ConsT (assocM n e mne) (asociarJ (jugadores e) n mnn) (insertPQ e pqe)

asociarJ :: [Nombre] -> Nombre -> Map Nombre Nombre -> Map Nombre Nombre -- O(J*(log J)) donde por cada jugador J hago dos operaciones logaritmicas donde a medida la estructura va creciendo -> O(J log J)
--lookupM = O(log J) donde J es la cantidad de jugadores del map
--assocM = O(log J) donde J es la cantidad de jugadores del map
asociarJ [] ne mnn       = mnn
asociarJ (nj:njs) ne mnn = case (lookupM nj mnn) of
                        Nothing  -> asociarJ njs ne (assocM nj ne mnn)
                        Just ne' -> error "Éste jugador ya posee equipo"

equipos :: Torneo -> [Equipo] --O(E log E) por equiposDe
--Propósito: denota la lista de equipos del torneo.
equipos (ConsT mne mnn pqe) = equiposDe pqe

equiposDe :: PriorityQueue Equipo -> [Equipo] --O(E*(log E)) por cada equipo E en el peor caso hace dos operaciones constante y una logaritmica -> O(E log E)
--isEmptyPQ = O(1)
--maxPQ = O(1)
--deleteMaxPQ = O(log E) donde E es la cantidad de equipos de la priority queue
equiposDe pqe = if isEmptyPQ pqe
                then []
                else maxPQ pqe : equiposDe (deleteMaxPQ pqe)




registrarGol :: Nombre -> Nombre -> Torneo -> Torneo --O(log J + log JE + E log E)
--O(log J) donde J es la cantidad de jugadores del equipo
--O(log E) donde E es la cantidad de equipos del map
--anotarGol = O(log JE) donde JE es la cantidad de jugadores del equipo
--assocM = O(log E) donde E es la cantidad de equipos del map
--modificarPQ = O(E log E)
--Propósito: dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo dado.
--Precondición: existe un jugador y un equipo con dichos nombres.
registrarGol nj ne (ConsT mne mnn pqe) = case (lookupM nj mnn) of
                                        Nothing  -> error "El jugador no existe"
                                        Just ne' -> if (ne == ne') 
                                                    then case (lookupM ne' mne) of
                                                            Nothing -> error "El equipo no existe"
                                                            Just e  -> let ecg = anotarGol nj e in
                                                                ConsT (assocM ne ecg mne) mnn (modificarPQ ecg pqe)
                                                    else error "No es del equipo dado"

modificarPQ :: Equipo -> PriorityQueue Equipo -> PriorityQueue Equipo -- O(E*(log E)) en peor caso por cada equipo E hace 3 operaciones constantes y 2 operaciones logaritmicas -> O(E log E)
--isEmptyPQ = O(1)
--emptyPQ = O(1)
--maxPQ = O(1)
--nombre = O(1)
--insertPQ = O(log E) donde E es la cantidad de equipos de la priority queue
--deleteMaxPQ = O(log E) donde E es la cantidad de equipos de la priority queue
modificarPQ e pqe = if isEmptyPQ pqe
                    then emptyPQ
                    else let e' = maxPQ pqe in
                        if (nombre e == nombre e')
                        then insertPQ e (deleteMaxPQ pqe)
                        else insertPQ e' (modificarPQ e (deleteMaxPQ pqe))



ingresarJugador :: Nombre -> Nombre -> Torneo -> Torneo -- costo de la operación = O(E log E + log J + log JF)
--lookupM = O(log E) donde E es la cantidad de equipos del map
--lookupM = O(log J) donde J es la cantidad de jugadores del map
--fichar = O(log JF) donde JF es la cantidad de nombre jugadores del equipo
--assocM = O(log E) donde E es la cantidad de equipos del map
--assocM = O(log J) donde J es la cantidad de jugadores del map
--actualizarE = O(E log E)
--Propósito: dado un nombre de jugador y un nombre de equipo, ingresa al torneo dicho jugador, con cero goles, agregándolo al equipo dado.
ingresarJugador nj ne (ConsT mne mnn pqe) = case (lookupM ne mne) of
                                            Nothing -> error "El equipo no existe"
                                            Just e  -> case (lookupM nj mnn) of
                                                        Nothing  -> let ejf = fichar nj e in
                                                            ConsT (assocM ne ejf mne) (assocM nj ne mnn) (actualizarE ejf pqe)
                                                        Just ne' -> error "Ya se encuentra en un equipo"


actualizarE :: Equipo -> PriorityQueue Equipo -> PriorityQueue Equipo --O(E*(log E)) donde por cada equipo E en peor caso hace operaciones constantes y operaciones logaritmicas -> O(E log E)
--isEmptyPQ = O(1)
--emptyPQ = O(1)
--maxPQ = O(1)
--nombre = O(1)
--insertPQ = O(log E) donde E es la cantidad de equipos de la priority queue
--deleteMaxPQ = O(log E) donde E es la cantidad de equipos de la priority queue
actualizarE e pqe = if isEmptyPQ pqe
                    then emptyPQ
                    else let e' = maxPQ pqe in
                        if (nombre e == nombre e')
                        then insertPQ e (deleteMaxPQ pqe)
                        else insertPQ e' (actualizarE e (deleteMaxPQ pqe))


data Torneo = ConsT (Map Nombre Equipo)
            -- nombreDeEquipo 
                    (Map Nombre Nombre)
            --nombreDeJugadores -nombreDeEquipo 
                    (PriorityQueue Equipo)

sinEquipoGoleador :: Torneo -> Torneo --costo de la operación = O(log E + NJ log J)
--deleteM = O(log E) donde E es la cantidad de equipos del map
--borrarJ = O(NJ log J)
--deleteMaxPQ = O(log E) donde E es la cantidad de equipos de la priority queue
--Propósito: devuelve un torneo donde se ha quitado al equipo con más goles anotados.
sinEquipoGoleador (ConsT mne mnn pqe) = if isEmptyPQ pqe
                                        then error "No existe equipo en el torneo"
                                        else let eq = maxPQ pqe
                                            nombreEq = nombre eq in
                                                ConsT (deleteM nombreEq mne) (borrarJ (jugadores eq) mnn) (deleteMaxPQ pqe) 


borrarJ :: [Nombre] -> Map Nombre Nombre -> Map Nombre Nombre --O(NJ*(log J)) donde por cada NJ hace una operación logaritmica, siendo NJ la longitud de la lista de nombres de jugadores del equipo
--deleteM = O(log J) donde J es la cantidad de jugadores del map
borrarJ [] mnn       = mnn
borrarJ (nj:njs) mnn = borrarJ njs (deleteM nj mnn)


registrarGol :: Nombre -> Nombre -> Torneo -> Torneo
Propósito: dados un nombre de jugador y un nombre de equipo, ingresa un gol anotado por el jugador dado para el equipo
dado.


anotarGoles :: [(Nombre, Nombre)] -> Torneo -> Torneo --O(N*(log J + log JE + E log E)) donde por cada elemento n de la lista, hace una operación de costo (log J + log JE + E log E) donde N es equivalente a la longitud de la lista
--registrarGol = O(log J + log JE + E log E)
--Propósito: dada una lista de pares de nombre de jugador (primera componente) y nombre de equipo (segunda componente), anota un gol en el torneo por cada elemento en la lista.
anotarGoles [] t            = t
anotarGoles (njne:njsnes) t = let (nj, ne) = njne in
    anotarGoles njsnes (registrarGol nj ne t)




mejoresEquipos :: Int -> Torneo -> [Equipo] -- O(E*(log E + NJ log J)) en peor caso la recursión se ejecuta una vez por cada equipo extraído en peor caso se piden todos los equipos entonces cantidad de iteraciones = E
--equipoGoleador = O(1)
--sinEquipoGoleador = O(log E + NJ log J)
--Propósito: dado un número n denota a los n equipos más goleadores, ordenados por cantidad de goles de mayor a menor.
--Precondición: existen al menos n equipos en el torneo.
mejoresEquipos 0 t = []
mejoresEquipos n t = equipoGoleador t : mejoresEquipos (n-1) (sinEquipoGoleador t)

jugadoresYGoles :: Torneo -> [(Nombre, Int)] -- O(J*(log J + log E + log JE))
--Propósito: denota la lista de jugadores del torneo junto con sus respectivos goles.
jugadoresYGoles t = jugadoresConGolesDelTorneo (jugadores t) t

jugadoresConGolesDelTorneo :: [Nombre] -> Torneo -> [(Nombre, Int)] -- O(J*(log J + log E + log JE))
--equipoDe = O(log J + log E)
--golesDe = O(log JE) donde JE es la cantidad de jugadores del equipo
jugadoresConGolesDelTorneo [] t       = []
jugadoresConGolesDelTorneo (nj:njs) t = case (equipoDe nj t) of
                                        Nothing -> error "El jugador no es de ningún equipo"
                                        Just e  -> let cantGoles = golesDe nj e in
                                            (nj, cantGoles) : jugadoresConGolesDelTorneo njs t