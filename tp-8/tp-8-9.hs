--EmGame 

--P es la cantidad de personajes en el juego
--E la cantidad de esmeraldas.

--1.

data Comando = IniciarJuego (Set Personaje) (Set Esmeralda)
            |  ObtenerEsmeralda Personaje Esmeralda        
            |  CompetirPor Personaje Personaje Esmeralda   
            |  UsarEsmeralda Personaje Esmeralda           


{-
Hacer:
partida :: [Comando] → Personaje
Dada una lista de comandos valida, describe el ganador del juego que resulta luego de ejecutar la lista. Falla si la lista de comandos no es valida, establecer eficiencia y justificar.
Una lista de comandos es valida si su primer elemento es IniciarJuego y los diferentes comandos cumplen las precondiciones de los comandos que representan al momento de ser ejecutados.
REQUISITO: La solucion debe realizarse por recursion estructural, para lo cual dicha recursion debe hacerse en una funcion auxiliar,sobre el reverse de la lista dada.
SUGERENCIA: para fallar, procesar la lista de comandos y dejar que las operaciones del TAD fallen si no se cumplen sus precondiciones.
-}

partida :: [Comando] -> Personaje --Costo final de la operación = O(C*(log P + E) + P log P + E log E)
--reverse = O(C) siendo C la cantidad de comandos de la lista
--juegoEm = O(C*(log P + E) + P log P + E log E)
partida cs = elMasPoronga (juegoEm (reverse cs))

juegoEm :: [Comando] -> EmGame -- O(C*(log P + E) + P log P + E log E)
--iniciar = O(P log P + E log E) 
--ejecutar =
juegoEm []     = error "No hay comandos en la lista para el juego"
juegoEm (c:cs) = if (null cs)
                then iniciar c
                else ejecutar c (juegoEm cs)

ejecutar :: Comando -> EmGame -> EmGame -- costo de la operación = O(log P + E)
--obtenerEsmeralda = O(log P + log E) donde P es la cantidad de personajes del juego y E la cantidad de esmeraldas del juego
--ganarEsmeralda = O(log P + E) donde P es la cantidad de personajes del juego y E la totalidad de esmeraldas del juego
--usarEsmeralda = O(log P + E) donde P es la cantidad de personajes del juego y E la totalidad de esmeraldas del juego
ejecutar (IniciarJuego _ _) _      = error "Comando inválido para ejecutar"
ejecutar (ObtenerEsmeralda p e) em = obtenerEsmeralda em p e
ejecutar (CompetirPor p1 p2 e) em  = ganarEsmeralda em p1 p2 e
ejecutar (UsarEsmeralda p e) em    = usarEsmeralda em p e

iniciar :: Comando -> EmGame --O(P log P + E log E) donde P es la cantidad de personajes del juego y E la cantidad de esmeraldas
iniciar (IniciarJuego sp se) = iniciarJuego sp se
iniciar _                    = error "Juego inválido"

{-
2)
TAD EmGame, Escribir INV.REP.
Data EmGame = AG (Map Personaje [Esmeralda])
(Map Esmeralda (Maybe Personaje))
(MaxHeap Personaje)
El primer Map tiene todos los personajes del juego con las esmeraldas de cada uno, el segundo,todas las esmeraldas del juego y si son poseidas, quien las posee y el tercero todos los personajes del juego ordenados por mayor poder.
-}

Data EmGame = AG (Map Personaje [Esmeralda])
                (Map Esmeralda (Maybe Personaje))
                (MaxHeap Personaje)
    {-
    INV REPR: En (AG mpes memp mhp) se cumple que:
            *En mpes, cada Personaje P asociado una lista de Esmeralda es, cada Esmeralda e en es pertenece únicamente a p
            *En mpes, cada Personaje P asociado a una lista de Esmeralda es, cada Esmeralda e asociado a p se encuentra como clave en memp y además tiene como valor a (Just p)
            *En memp, cada Esmeralda e asociado a un (Just p), p se encuentra como clave en mpes y además e pertenece a la lista de Esmeralda, si e asocia a un (Nothing), e no pertenece a ningún Personaje en mpes
            *En mhp, cada Personaje p se encuentra como clave en mpes. Y cada clave Personaje en mpes, se encuentra en mhp
            *mhp está ordenado de mayor a menor correspondiente al poder del Personaje
    -}

{-
3) Implementar funciones respetando proposito, y que las fallas que tenga un mensaje de error, cumpliendo con la eficiencia y justificando.
-}


iniciarJuego :: Set Personaje → Set Esmeralda → EmGame --costo de la operación = O(P log P + E log E)
--sizeS = O(1)
--set2list = O(P) siendo P todos los personajes del set
--set2list = O(E) siendo E todas las esmeraldas del set
--agregarPersonajesM = O(P log P)
--agregarEsmeraldasM = O(E log E)
--agregarPersonajesH = O(P log P)
--dado un conjunto de personajes y un conjunto de esmeraldas (suponiendo ambos no vacios) describe un juego inicial con esas esmeraldas y esos personajes sin esmeraldas. Falla si alguno de los conjuntos esta vacio (O(P log P + E log E))
iniciarJuego sp se = 
        if (sizeS sp == 0 || sizeS se == 0)
        then error "No hay personajes o esmeraldas para el juego"
        else let ps = set2list sp
                es = set2list se in
                        AG (agregarPersonajesM ps) (agregarEsmeraldasM es) (agregarPersonajesH ps)

agregarPersonajesM :: [Personaje] -> Map Personaje [Esmeralda] -- O(P*(log P)) por cada personaje P en la lista hace una operación logaritmica y el map va creciendo, costo final de la operación = O(P log P) siendo P la longitud de la lista
--assocM = O(log P) donde P es la cantidad de personajes del juego
agregarPersonajesM []     = emptyM
agregarPersonajesM (p:ps) = assocM p [] (agregarPersonajesM ps)

agregarEsmeraldasM :: [Esmeralda] -> Map Esmeralda (Maybe Personaje) -- O(E*(log E)) donde por cada esmeralda E de la lista, hace una operación logaritmica, costo de la operación = O(E log E) siendo E la longitud de la lista
--assocM = O(log E) donde E es la cantidad de esmeraldas del map
agregarEsmeraldasM []     = emptyM
agregarEsmeraldasM (e:es) = assocM e Nothing (agregarEsmeraldasM es)

agregarPersonajesH :: [Personaje] -> MaxHeap Personaje -- O(P*(log P)) donde cada personaje P hace una operación logaritmica y va creciendo en cada iteración, costo de la operación = O(P log P) siendo P la longitud de la lista
--assocM = O (log P) donde P es la cantidad de personajes de la heap
agregarPersonajesH []     = emptyH
agregarPersonajesH (p:ps) = insertH p (agregarPersonajesH ps)

elMasPoronga :: EmGame → Personaje
--findMaxH = O(1)
--dado un juego describe el mas poderoso (O(1))
elMasPoronga (AG mpes memp mhp) = findMaxH mhp



esmeraldasDe :: EmGame -> Personaje -> [Esmeralda] -- costo de la operación = O(log P)
--lookupM = O(log P) donde P es la cantidad de personajes del map
--dado 1 juego y un personaje,describe la lista de esmeraldas de ese personaje. Falla si el personaje no esta en el juego (O(log P))
esmeraldasDe (AG mpes memp mhp) p = case (lookupM p mpes) of
                                Nothing -> error "El personaje no existe"
                                Just es -> es

obtenerEsmeralda :: EmGame → Personaje -> Esmeralda -> EmGame --costo de la operación = O(log P + log E)
--lookupM = O(log P) donde P es la cantidad de personajes del map
--lookupM = O(log E) donde E es la cantidad de esmeraldas del map
--estaAsignada = O(1)
--assocM = O(log P) donde P es la cantidad de personajes del map
--assocM = O(log E) donde E es la cantidad de esmeraldas del map
{-dado un juego,personaje y esmeralda dentro de ese juego,describe el resultado de asignar la esmeralda al personaje suponiendo que esa esmeralda no esta asignada. Falla si el personaje o la esmeralda no esta en el juego-}
obtenerEsmeralda (AG mpes memp mhp) p e = case (lookupM p mpes) of
                                        Nothing -> error "El personaje no está en el juego"
                                        Just es -> case (lookupM e memp) of
                                                Nothing -> error "La esmeralda no está en el juego"
                                                Just mp -> if (estaAsignada mp)
                                                        then error "La esmeralda ya está asignada"
                                                        else AG (assocM p (e:es) mpes) (assocM e (Just p) memp) mhp

estaAsignada :: Maybe Personaje -> Bool --O(1)
estaAsignada Nothing  = False
estaAsignada (Just _) = True




--dado 1 juego,2 personajes y 1 esmeralda dentro de ese juego,describe el resultado de realizar la competencia entre ambos personajes,suponiendo que esa esmeralda la tiene alguno de los personajes,y se la queda el mas poderoso. Falla si los personajes o la esmeralda no son parte del juego, y si la esmeralda no la tiene uno de los 2 personajes
ganarEsmeralda :: EmGame -> Personaje -> Personaje -> Esmeralda -> EmGame --costo de la operación = O(log P + E)
--lookupM = O(log P) donde P es la cantidad de personajes del map
--lookupM = O(log P) donde P es la cantidad de personajes del map
--lookupM = O(log E) donde E es la cantidad de esmeraldas del map
--quitarEsmeralda = O(E) donde E es la cantidad de esmeraldas del personaje
--quitarEsmeralda = O(E) donde E es la cantidad de esmeraldas del personaje
--assocM = O(log P) donde P es la cantidad de personajes del map
ganarEsmeralda (AG mpes memp mhp) p1 p2 e = case (lookupM p1 mpes) of
                                        Nothing -> error "No existe el personaje en el juego"
                                        Just es1 -> case (lookupM p2 mpes) of
                                                Nothing  -> error "No existe el personaje en el juego"
                                                Just es2 -> case (lookupM e memp) of
                                                        Nothing -> error "La esmeralda no existe"
                                                        Just mp -> if (esPoseidaPor mp p1 || esPoseidaPor mp p2)
                                                                then let es1' = quitarEsmeralda e es1
                                                                        es2' = quitarEsmeralda e es2
                                                                        g = ganadorDe p1 p2 in
                                                                        if (g == p1)
                                                                        then let mpes' = assocM p2 es2' mpes in
                                                                                AG (assocM p1 (e:es1') mpes') (assocM e (Just p1) memp) mhp
                                                                        else let mpes' = assocM p1 es1' mpes in
                                                                                AG (assocM p2 (e:es2') mpes') (assocM e (Just p2) memp) mhp
                                                                else error "Ningún personaje posee la esmeralda"


esPoseidaPor :: Maybe Personaje -> Personaje -> Bool -- O(1)
esPoseidaPor (Just p) p' = p == p'
esPoseidaPor Nothing _   = False

ganadorDe :: Personaje -> Personaje -> Personaje -- O(1)
ganadorDe p1 p2 = if (poder p1 > poder p2)
                then p1
                else p2

quitarEsmeralda :: Esmeralda -> [Esmeralda] -> [Esmeralda] -- O(E)
--quitarEsmeralda = O(E) siendo E todas las esmeraldas de la lista
quitarEsmeralda e []      = []
quitarEsmeralda e (e':es) = if (e == e')
                                then es
                                else e' : quitarEsmeralda e es


Data EmGame = AG (Map Personaje [Esmeralda])
                (Map Esmeralda (Maybe Personaje))
                (MaxHeap Personaje)
{-
dado un juego,personaje,esmeralda (dentro de ese juego)describe el juego que resulta de la ultilizacion de su esmeralda por el personaje,suponiendo que el personaje tiene la esmeralda. Falla si el personaje o la esmeralda no son parte del juego, y si la esmeralda no la tiene el personaje
-}
usarEsmeralda :: EmGame → Personaje → Esmeralda → EmGame --costo de la operación = O(P log P + E)
--lookupM = O(log P) donde P es la cantidad de personajes del map
--lookupM = O(log E) donde E es la cantidad de esmeraldas del map
--esPoseidaPor = O(1)
--aumentarPoderDe = O(1)
--quitarEsmeralda = O(E) donde e es la cantidad de esmeraldas de la lista
--deleteM = O(log P) donde P es la cantidad de personajes del map
--assocM = O(log P) donde P es la cantidad de personajes del map
--deleteM = O(log P) donde P es la cantidad de personajes del map
--actualizarH = O(P log P)
usarEsmeralda (AG mpes memp mhp) p e = case (lookupM p mpes) of
                                        Nothing -> error "El personaje no existe"
                                        Just es -> case (lookupM e memp) of
                                                Nothing -> error "La esmeralda no existe"
                                                Just mp -> if (esPoseidaPor mp p)
                                                        then let p' = aumentarPoderDe p (poder p)
                                                                es' = quitarEsmeralda e es
                                                                mpes' = deleteM p mpes in
                                                                        AG (assocM p' es' mpes') (deleteM e memp) (actualizarH p p' mhp)
                                                        else error "El personaje no posee la esmeralda"


actualizarH :: Personaje -> Personaje -> MaxHeap Personaje -> MaxHeap Personaje -- costo de la operación = O(P*(log P)) en peor caso por cada P personaje se hacen operaciones logaritmicas -> O(P log P)
--isEmptyH = O(1)
--findMaxH = O(1)
--insertH = O(log P) donde P es la cantidad de personajes de la heap
--deleteMaxH = O(log P) donde P es la cantidad de personajes de la heap
actualizarH p1 p2 mhp = if isEmptyH mhp
                   then emptyH
                   else let p' = findMaxH mhp in
                        if (p1 == p')
                        then insertH p2 (deleteMaxH mhp)
                        else insertH p' (actualizarH p1 p2 (deleteMaxH mhp))