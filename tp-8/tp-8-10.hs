{-
Parcial Modelo – Plataforma de Cursos Online

Una plataforma administra cursos, alumnos y puntos obtenidos.

Cada curso tiene nombre único.
Cada alumno tiene nombre único.
Un alumno puede estar inscripto en un solo curso.
Los alumnos suman puntos al completar actividades.
Un curso “vale más” cuanto mayor sea la suma de puntos de sus alumnos.

Se supone existentes los tipos:
type Nombre = String

Y un TAD ya implementado:

TAD Curso

Un curso posee nombre, alumnos y puntos acumulados por alumno.

Interfaz de Curso

crearCurso :: Nombre -> Curso -- O(1)

nombreCurso :: Curso -> Nombre -- O(1)

inscribir :: Nombre -> Curso -> Curso -- O(log A)

alumnosCurso :: Curso -> [Nombre] -- O(1)

sumarPuntos :: Nombre -> Int -> Curso -> Curso -- O(log A)

puntosDe :: Nombre -> Curso -> Int -- O(log A)

puntosTotales :: Curso -> Int -- O(1)

Dos cursos se comparan por puntosTotales.

TAD Plataforma

Sea:

data Plataforma = PL (Map Nombre Curso)
                    --nombreCurso
                    (Map Nombre Nombre)
                --nombreAlumno --mombreCurso
                    (PriorityQueue Curso)

Donde:

Primer Map: nombreCurso → Curso
Segundo Map: alumno → nombreCurso
PriorityQueue: cursos ordenados por puntosTotales

Ejercicios
A) Invariantes de representación
Dar invariantes válidos para Plataforma.
Puntaje: 1

Implementación del TAD Plataforma

Implementar justificando costos.

B)
curso :: Nombre -> Plataforma -> Maybe Curso
Propósito: dado un nombre de curso devuelve ese curso.
Costo esperado: O(log C)

C)
cursoDe :: Nombre -> Plataforma -> Maybe Curso
Propósito: dado un alumno devuelve el curso en el que está.
Costo esperado: O(log A + log C)

D)
alumnos :: Plataforma -> [Nombre]
Propósito: devuelve todos los alumnos registrados.
Costo esperado: O(A)

E)
mejorCurso :: Plataforma -> Curso
Propósito: devuelve el curso con mayor puntaje total.
Precondición: existe al menos un curso.
Costo esperado: O(1)

F)
iniciarPlataforma :: [Curso] -> Plataforma
Propósito: inicia plataforma con esos cursos ya cargados.
Costo esperado: O(N²)

G)
registrarAlumno :: Nombre -> Nombre -> Plataforma -> Plataforma
Propósito: dado alumno y curso, lo inscribe al curso.
Precondición: no existe alumno repetido.
Costo esperado: O(N log N)

H)
registrarActividad :: Nombre -> Int -> Plataforma -> Plataforma
Propósito: suma puntos al alumno dado.
Costo esperado: O(N log N)

I)
quitarMejorCurso :: Plataforma -> Plataforma
Propósito: elimina el curso con más puntos. Debe borrar también sus alumnos.
Costo esperado: O(N log N)

Usuario del TAD Plataforma

J)
sumarActividades :: [(Nombre, Int)] -> Plataforma -> Plataforma
Propósito: Dada lista (alumno,puntos) suma todos los puntos.

K)
topCursos :: Int -> Plataforma -> [Curso]
Propósito: Devuelve los n mejores cursos ordenados.

L)
alumnosYPuntos :: Plataforma -> [(Nombre, Int)]
Propósito: Devuelve cada alumno con sus puntos actuales.

Representación de Curso
M) Dar una posible representación para Curso que permita cumplir la interfaz.

-}

type Nombre = String


data Plataforma = PL (Map Nombre Curso)
                    --nombreCurso
                    (Map Nombre Nombre)
                --nombreAlumno --mombreCurso
                    (PriorityQueue Curso)
        {-
        INV REPR: En (PL mnc mnn pqc) se cumple que:
                *En mnc, cada Nombre nc asociado a un Curso c, el nombre de c es idéntico a nc
                *En mnn, cada Nombre na asociado a un Nombre nc, nc se encuentra como clave en mnc, donde na pertenece a la lista de alumnos del Curso asociado
                *En mnc, cada Nombre nc asociado a un Curso c, c es idéntico al Curso en pqc. Cada Curso en pqc, es idéntico a c
                *pqc está ordenado de mayor a menor correspondiente a los puntos totales del Curso.
        -}

curso :: Nombre -> Plataforma -> Maybe Curso -- O(log C)
--O(log C) donde C es la cantidad de curso del map
--Propósito: dado un nombre de curso devuelve ese curso.
--Costo esperado: O(log C)
curso nc (PL mnc mnn pqc) = lookupM nc mnc

cursoDe :: Nombre -> Plataforma -> Maybe Curso --O(log A + log C)
--lookupM = O(log A) donde A es la cantidad de alumnos del map
--lookupM = O(log C) donde C es la cantidad de cursos del map
--Propósito: dado un alumno devuelve el curso en el que está.
cursoDe na (PL mnc mnn pqc) = case (lookupM na mnn) of
                            Nothing -> Nothing
                            Just nc -> lookupM nc mnc
                                
alumnos :: Plataforma -> [Nombre] --O(A) donde A es la cantidad total de los alumnos registrados
--Propósito: devuelve todos los alumnos registrados.
alumnos (PL mnc mnn pqc) = domM mnn

mejorCurso :: Plataforma -> Curso --O(1)
--Propósito: devuelve el curso con mayor puntaje total.
--Precondición: existe al menos un curso.
mejorCurso (PL mnc mnn pqc) = if isEmptyPQ pqc
                            then error "No existe un curso en la plataforma"
                            else findMaxPQ pqc



iniciarPlataforma :: [Curso] -> Plataforma -- O(C*(log C)) donde por cada curso C hago dos operaciones logaritmicas, donde las estructuras van creciendo en cada iteración, siendo C la longitud de la lista -> O(C *(log C + A log A))
--nombreCurso = O(1)
--assocM = O(log C) donde C es la cantidad de cursos del map donde va creciendo en cada iteración
--insertarAlumnos = O(A log A)
--alumnosCurso = O(1)
--insertPQ = O(log C) donde C es la cantidad de cursos de la priorityqueue donde la estructura va creciendo en cada iteración
--Propósito: inicia plataforma con esos cursos ya cargados.
iniciarPlataforma []     = PL emptyM emptyM emptyPQ
iniciarPlataforma (c:cs) = case (iniciarPlataforma cs) of
                            pl -> let n = nombreCurso c 
                                    (PL mnc mnn pqc) = pl in
                                PL (assocM n c mnc) (insertarAlumnos (alumnosCurso c) n mnn) (insertPQ c pqc)


insertarAlumnos :: [Nombre] -> Nombre -> Map Nombre Nombre -> Map Nombre Nombre --O(a*(log A)) donde por cada alumno a, hace una operación logaritmica -> O(a log A)
--assocM = O(log A) donde A es la cantidad de alumnos del map
insertarAlumnos [] nc mnn       = mnn
insertarAlumnos (na:nas) nc mnn = insertarAlumnos nas nc (assocM na nc mnn)

data Plataforma = PL (Map Nombre Curso)
                    --nombreCurso
                    (Map Nombre Nombre)
                --nombreAlumno --mombreCurso
                    (PriorityQueue Curso)

registrarAlumno :: Nombre -> Nombre -> Plataforma -> Plataforma -- costo de la operación = O(log A + C log C + log AC)
--lookupM = O(log A) donde A es la cantidad de alumnos del map
--lookupM = O(log C) donde C es la cantidad de cursos del map
--assocM = O(log C) donde C es la cantidad de cursos del map
--assocM = O(log A) donde A es la cantidad de alumnos del map
--inscribir = O(log AC) donde AC es la cantidad de alumnos del curso
--modificarPQ = O(C log C)
--Propósito: dado alumno y curso, lo inscribe al curso.
--Precondición: no existe alumno repetido.
registrarAlumno na nc (PL mnc mnn pqc) = case (lookupM na mnn) of
                                        Just nc -> error "El alumno ya tiene un curso registrado"
                                        Nothing -> case (lookupM nc mnc) of
                                                Nothing -> error "El curso para registrar al alumno no existe"
                                                Just c  -> let c' = inscribir na c in
                                                    PL (assocM nc c' mnc) (assocM na nc mnn) (modificarPQ c' pqc)


modificarPQ :: Curso -> PriorityQueue Curso -> PriorityQueue Curso --O(C*(log C)) donde por cada curso C en peor caso, unas operaciones constantes y logaritmicas por cada c de la priorityqueue -> O(C log C)
--isEmptyPQ = O(1)
--emptyPQ = O(1)
--findMaxPQ = O(1)
--insertPQ = O(log C) donde C es la cantidad de cursos de la priorityqueue
--deleteMaxPQ = O(log C) donde C es la cantidad de cursos de la priorityqueue
modificarPQ c pqc = if isEmptyPQ pqc
                    then emptyPQ
                    else let c' = findMaxPQ pqc in
                        if (nombre c == nombre c')
                        then insertPQ c (deleteMaxPQ pqc)
                        else insertPQ c' (modificarPQ c (deleteMaxPQ pqc))


registrarActividad :: Nombre -> Int -> Plataforma -> Plataforma -- O(C log C + log A + log AC)
--lookupM = O(log A) donde A es la cantidad de alumnos del map
--lookupM = O(log C) donde C es la cantidad de cursos del map
--sumarPuntos = O(log AC) donde AC es la cantidad de alumnos de un curso
--assocM = O(log C) donde C es la cantidad de cursos del map
--modificarPQ = O(C log C)  
--Propósito: suma puntos al alumno dado.
registrarActividad na p (PL mnc mnn pqc) = case (lookupM na mnn) of
                                        Nothing -> error "El alumno no existe"
                                        Just nc -> case (lookupM nc mnc) of
                                            Nothing -> error "El curso no existe"
                                            Just c  -> let c' = sumarPuntos na p c in
                                                PL (assocM nc c' mnc) mnn (modificarPQ c' pqc)

quitarMejorCurso :: Plataforma -> Plataforma --O(log C + AC log A) donde AC es la cantidad de alumnos del curso
--findMaxPQ = O(1)
--nombreCurso = O(1)
--deleteM = O(log C) donde C es la cantidad de cursos del map
--alumnosCurso = O(1) 
--borrarAlumnos = O(AC log A) donde AC es la cantidad de alumnos del curso, y log A la cantidad de alumnos del map
--deleteMaxPQ = O(log C) donde C es la cantidad de cursos de la priorityqueue
--Propósito: elimina el curso con más puntos. Debe borrar también sus alumnos.
quitarMejorCurso (PL mnc mnn pqc) = if isEmptyPQ pqc
                                    then error "No hay cursos en la plataforma"
                                    else let mj = findMaxPQ pqc
                                            nc = nombreCurso mj in
                                                PL (deleteM nc mnc) (borrarAlumnos (alumnosCurso mj) mnn) (deleteMaxPQ pqc)

borrarAlumnos :: [Nombre] -> Map Nombre Nombre -> Map Nombre Nombre --O(A log A) en peor caso por cada alumno A se hace una operación logaritmica -> (A log A)
--deleteM = O(log A) donde A es la cantidad de alumnos del map
borrarAlumnos [] mnn       = mnn
borrarAlumnos (na:nas) mnn = borrarAlumnos nas (deleteM na mnn)

--USUARIO

sumarActividades :: [(Nombre, Int)] -> Plataforma -> Plataforma --O(N*(C log C + log A + log AC)) siendo N la cantidad de elementos de la lista
--registrarActividad = O(C log C + log A + log AC)
--Propósito: Dada lista (alumno,puntos) suma todos los puntos.
sumarActividades [] pl         = pl
sumarActividades (nps:npss) pl = let (n, ps) = nps in
    sumarActividades npss (registrarActividad n ps pl)

topCursos :: Int -> Plataforma -> [Curso] --O(N*(log C + AC log A)) donde por cada N hace una operación log C + AC log A, tal que N es la cantidad de iteraciones
--mejorCurso = O(1)
--quitarMejorCurso = O(log C + AC log A)
--Propósito: Devuelve los n mejores cursos ordenados.
topCursos 0 pl = []
topCursos n pl = mejorCurso pl : topCursos (n-1) (quitarMejorCurso pl)

alumnosYPuntos :: Plataforma -> [(Nombre, Int)] --costo de la operación = O(A* (log A + log C + log AC))
--alumnosConPuntos = O(A log AC)
--alumnosYCursos = O(A*(log A + log C))
--Propósito: Devuelve cada alumno con sus puntos actuales.
alumnosYPuntos pl = alumnosConPuntos (alumnosYCursos (alumnos pl) pl) pl 

alumnosYCursos :: [Nombre] -> Plataforma -> [(Nombre, Curso)] --O(N*(log A + log C)) por cada N nombres alumnos se hace una operación log A + log C por cada iteración que en peor caso son todos los alumnos de la plataforma -> O(A*(log A + log C))
--cursoDe = O(log A + log C)
alumnosYCursos [] pl       = []
alumnosYCursos (na:nas) pl = case (cursoDe na pl) of
                            Nothing -> error "El alumno no tiene curso"
                            Just c  -> (na, c) : alumnosYCursos nas pl 

alumnosConPuntos :: [(Nombre, Curso)] -> [(Nombre, Int)] --O(N*(log AC)) donde por cada N nombre alumno hace una operación logaritmica, que en peor caso son todos los alumnos de la plataforma -> O(A log AC)
--puntosDe = O(log AC) donde AC es la cantidad de alumnos del curso
alumnosConPuntos []         = []
alumnosConPuntos (nac:nacs) = let (na, c) = nac
                                ps = puntosDe na c in
                                    (na, ps) : alumnosConPuntos nacs



data Curso = C Nombre [Nombre] (Map Nombre Int) Int
    --nombreCurso --[listaDeAlumnos] (Map nombreAlumno puntos) --puntos totales
    {-
    INV REPR: En (C nc as mnp pt) se cumple que:
            *En as no hay elementos repetidos
            *En mnp, cada Nombre a asoaciado a un Int p, pt es igual a la suma de todos los valores p almacenados en mnp
            *En as, cada Nombre a aparece como clave en mnp
            *En mnp, cada Nombre a asociado a un Int p, a aparece como elemento en as
    -}

crearCurso :: Nombre -> Curso -- O(1)

nombreCurso :: Curso -> Nombre -- O(1)

inscribir :: Nombre -> Curso -> Curso -- O(log A)

alumnosCurso :: Curso -> [Nombre] -- O(1)

sumarPuntos :: Nombre -> Int -> Curso -> Curso -- O(log A)

puntosDe :: Nombre -> Curso -> Int -- O(log A)

puntosTotales :: Curso -> Int -- O(1)