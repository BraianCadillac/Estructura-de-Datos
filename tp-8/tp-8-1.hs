
{-
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

- El tipo Sector es un tipo abstracto, y representa al sector de una nave, el cual contiene componentes y tripulantes asignados.
- El tipo Tripulante es un tipo abstracto, y representa a un tripulante dentro de la nave, el cual tiene un nombre, un rango
    y sectores asignados.
- El tipo SectorId es sinónimo de String, e identifica al sector de forma unívoca.
- Los tipos Nombre y Rango son sinónimos de String. Todos los nombres de tripulantes son únicos.
- Un sector está vacío cuando no tiene tripulantes, y la nave está vacía si no tiene ningún tripulante.
- Puede haber tripulantes sin sectores asignados.

Ejercicios
Invariantes

a) Dar invariantes de representación válidos según la descripción de la estructura.
-}

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
    {-
    INV REPR: En (N mss mnt mt) se cumple que:
        *Para cada clave valor en mss, la clave SectorId coincide con el valor Sector asociado
        *Para cada clave valor en mnt, la clave Nombre coincide con el valor Tripulante asociado
        *La cantidad de tripulantes en mt, es igual a la cantidad de clave-valor en mnt
        *En mt se cumple que, no hay tripulantes repetidos
        *En mt, cada tripulante se encuentra en mnt
        *En mnt, cada tripulante se encuentra en mt
        *En mss, todos los tripulantes asignados a cada sector, se encuentran en mnt
        *En mnt, todos los sectores asignados a un tripulante, se encuentran en mss
    -}

{-
Implementación
Implementar la siguiente interfaz de Nave, utilizando la representación y los costos dados, calculando los costos de cada
subtarea, y siendo T la cantidad de tripulantes y S la cantidad de sectores:
-}

construir :: [SectorId] -> Nave -- O(S log S) con respecto a ingresarSectores
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
construir sids = N (ingresarSectores sids) emptyM emptyH

ingresarSectores :: [SectorId] -> Map SectorId Sector -- O(S log S) siendo S la cantidad de sectores de la lista, por cada S hacemos una operación O(log S)
ingresarSectores []     = emptyM
ingresarSectores (s:ss) = assocM s (crearS s) (ingresarSectores ss)


ingresarT :: Nombre -> Rango -> Nave -> Nave -- O(log T + log T + log T) en peor caso que el tripulante no esté. Hace un llamado a lookupM de costo O(log T), a assocM O(log T) siendo t la cantidad de tripulantes del map de ambos, y O(log T) siendo T la cantidad de tripulantes en la heap. Costo final de la operación = O(log T)
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
ingresarT n r (N mss mnt mt) = 
    let t = crearT n r in
        case (lookupM n mnt) of
            Nothing -> N mss (assocM n t mnt) (insertH t mt)
            Just e  -> error "El tripulante ya existe"



sectoresAsignados :: Nombre -> Nave -> Set SectorId -- Costo de la operación = O(log T)
--lookupM = O(log T) con respecto a T la cantidad de tripulantes en el map
--sectoresT = O(1)
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
sectoresAsignados n (N mss mnt mt) = case (lookupM n mnt) of
                                    Nothing -> error "No existe tripulante con el nombre dado"
                                    Just t  -> sectoresT t




datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente]) --Costo final = O(log S)
--lookupM = O (log S) con respecto a S la cantidad de sectores en el map, tripulantesS y componentesS son operaciones de orden 1 O(1)
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
datosDeSector si (N mss mnt mt) = case (lookupM si mss) of
                                Nothing -> error "No existe el sectorId dado"
                                Just s  -> (tripulantesS s, componentesS s)



tripulantesN :: Nave -> [Tripulante] -- O(T log T) por tripulantesDe
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
tripulantesN (N mss mnt mt) = tripulantesDe mt

tripulantesDe :: MaxHeap Tripulante -> [Tripulante] -- Por cada tripulante hago una operación O(log T) y O(1). Costo de la operación = O(T log T) 
--isEmptyH = O(1)
--maxH = O(1)
--deleteMaxH = O (log T) con respecto a T la cantidad de tripulantes en la Heap
tripulantesDe mt = if (isEmptyH mt)
                    then []
                    else maxH mt : tripulantesDe (deleteMaxH mt)


agregarASector :: [Componente] -> SectorId -> Nave -> Nave -- Costo de la operación = O(log S + C)
--lookupM = O(log S) con respecto a S la cantidad de sectores en el map
--assocM = O(log S) con respecto a S la cantidad de sectores en el map
--agregarComponentes = O(C)
--Propósito: Asigna una lista de componentes a un sector de la nave.
agregarASector cs si (N mss mnt mt) = 
        case (lookupM si mss) of
        Nothing -> error "No existe el sectorId dado en la Nave"
        Just s  -> N (assocM si (agregarComponentes cs s) mss) mnt mt

agregarComponentes :: [Componente] -> Sector -> Sector -- O(C) con respecto a C la cantidad de componentes en la lista hace una operación constante O(1) llamando a agregarC
agregarComponentes [] s     = s
agregarComponentes (c:cs) s = agregarC c (agregarComponentes cs s)


asignarASector :: Nombre -> SectorId -> Nave -> Nave -- costo de la operación = O(log S + T log T) -> T log T absorve las demás operaciones logarítmicas de O(log T)
--lookupM = O(log T) con respecto a T la cantidad de tripulantes en el map
--lookupM = O(log S) con respecto a S la cantidad de sectores en el map
-- assocM = O(log T) siendo T la cantidad de tripulantes en el map
-- asignarSectorA = O(T log T)
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
asignarASector n si (N mss mnt mt) = case (lookupM n mnt) of
                                    Nothing -> error "El nombre del tripulante no existe"
                                    Just t  -> case (lookupM si mss) of
                                                Nothing -> error "El sectorId dado no existe"
                                                Just s  -> N mss (assocM n (asignarS si t) mnt) (asignarSectorA si t mt)

asignarSectorA :: SectorId -> Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante -- Por cada T hace unas operaciones logarítimas O(log T), en el peor caso el tripulante puede estar al final -> costo de la operación = O(T log T)
--insertH = O(log T) con respecto a T la cantidad de tripulantes en la heap
--deleteMaxH = O(log T) con respecto a T la cantidad de tripulantes en la heap
--PRECOND: El tripulante se encuentra en la Heap
asignarSectorA si t mt = if (nombre (maxH mt) == nombre t)
                        then insertH (asignarS si t) (deleteMaxH mt)
                        else insertH (maxH mt) (asignarSectorA si t (deleteMaxH mt))


-------------------------------USUARIO----------------------

--Usuario
--Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:
sectores :: Nave -> Set SectorId -- O(T log T + T(S log S))
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores n = sectoresDe (tripulantesN n) n

sectoresDe :: [Tripulante] -> Nave -> Set SectorId -- O(T(S log S)) por cada T hago una operación de costo O(S log S) ts veces en respecto a la longitud de la lista, por cada elemento del set hace una insersión logarítmica S veces, siendo S los sectores de la empresa de la nave. Costo de la operación -> O(T * S log S)
sectoresDe [] n     = emptyS
sectoresDe (t:ts) n = unionS (sectoresAsignados (nombre t) n) (sectoresDe ts n)

sinSectoresAsignados :: Nave -> [Tripulante] -- O(T log T + T log T) = O(T log T)
--Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados n = sinSectores (tripulantesN n) n

sinSectores :: [Tripulante] -> Nave -> [Tripulante] -- O(T*(log T)) por cada tripulante hago una operación de costo O(log T) -> O(T log T)
-- sectoresAsignados = O(log T) siendo t la cantidad de tripulantes en la nave
-- sizeS = O(1)
sinSectores [] n     = []
sinSectores (t:ts) n = if (sizeS(sectoresAsignados (nombre t) n) == 0)  
                        then t : (sinSectores ts)
                        else (sinSectores ts)




barriles :: Nave -> [Barril] -- O(T log T + (T * (S log S)) + S log S + C + B)
--barrilesDe = O(S log S + C + B))
--sectores = O(T*(S log S)
--tripulantesN = O(T log T)
-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles n = barrilesDe (sectores (tripulantesN n) n) n


sectores :: [Tripulante] -> Nave -> Set SectorId -- O(T * (S log S))
--sectoresAsignados = O(log S) T con respecto a la cantidad de tripulantes de la nave
--unionS = O(T*(S log S))
sectores [] n     = emptyS
sectores (t:ts) n = unionS (sectoresAsignados (nombre t) n) (sectores ts n)


barrilesDe :: Set SectorId -> Nave -> [Barril]
--setToList -- O(S) S con respecto a la cantidad de todos los sectores del set
--barrilesDeLista = O(S log S + C + B))
barrilesDe sis n = barrilesDeLista (setToList sis) n

barrilesDeLista :: [SectorId] -> Nave -> [Barril] -- O(S log S + C + B)) por cada S en la lista se hace una operación O(log S), donde por cada si se concatena la lista de barriles que genera y el costo es proporcional a la suma de las longitudes de esas lista que genera cada si de la lista 
barrilesDeLista [] n       = []
barrilesDeLista (si:sis) n =
    let (_, cs) = datosDeSector si n
    in barrilesAca cs ++ barrilesDeLista sis n


barrilesAca :: [Componente] -> [Barril] -- O(C+B) por cada componente C, se concatena la lista de Barriles que genera y el costo de la operación es proporcional a la suma de las longitudes de esas listas que genera cada componente de la lista.
barrilesAca []     = []
barrilesAca (c:cs) = barrilesActual c ++ barrilesAca cs


barrilesActual :: Componente -> [Barril] -- O(1)
barrilesActual (Almacen bs) = bs
barrilesActual _            = []