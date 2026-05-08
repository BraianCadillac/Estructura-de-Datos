{-
Existe un tipo abstracto llamado Persona, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Una persona
posee un nombre, y una lista de evidencia en su contra, además de la cantidad. Adicionalmente, su interfaz permite ingresar
una evidencia.
El tipo Evidencia es sinónimo de String, y se corresponde con el nombre de una evidencia.
El tipo Nombre es sinónimo de String, y se corresponde con el nombre de una persona.
Podemos suponer que dos personas son iguales si poseen el mismo nombre, y una persona es más sospechosa que otra si
posee más evidencia en su contra (lo que permite ordenarlos por la cantidad de evidencia).
En la investigación no existen dos personas ni evidencias con el mismo nombre.
Representación
Dicho esto, la representación que utilizaremos será la siguiente (que no es posible modificar):

Esta representación utiliza:
Un Map que relaciona a personas con su nombre.
Un Map que relaciona una lista de nombres con una evidencia en su contra que las mismas comparten.
Una PriorityQueue que posee a todas las personas de la investigación, y que permite obtenerlas de forma eficiente de mayor
a menor en base a la cantidad de evidencia en su contra.
Un Int que indica la cantidad de evidencia diferente en la investigación.
-}

data Investigacion = ConsI (Map Nombre Persona)
                            --nombrePersona 
                            (Map Evidencia [Nombre])
                                        --nombrePersona
                            (PriorityQueue Persona)
                            Int

        {-
        INV.REPR: En (ConsI mnp mens pqp n) se cumple que:
                *En mnp, cada Nombre np asociado a una Persona p, el nombre de p es idéntico a np y además p pertenece a pqp.
                *En mens, cada Evidencia e asociado a una lista de nombres ns, donde ns no contiene elementos repetidos.
                *En mens, cada Evidencia e asociado a una lista de nombres ns, donde cada elemento Nombre n en ns se encuentra como clave en mnp.
                *En mnp, cada Nombre np asociado a una Persona p, cada evidencia de p se encuentran como clave en mens
                *En pqp, cada elemento Persona p, donde p se encuentra como valor en mnp.
                *pqp está ordenado de mayor a menor correspondiente a la cantidad de evidencias
                *n es igual a la cantidad de Evidencia en mens.
        -}

comenzarInvestigacion :: Investigacion -- O(1)
--Propósito: crea una investigación sin datos.
comenzarInvestigacion = ConsI emptyM emptyM emptyPQ 0

cantEvidenciaIngresada :: Investigacion -> Int -- O(1)
--Propósito: devuelve la cantidad de eviencia ingresada.
cantEvidenciaIngresada (ConsI mnp mens pqp n) = n

evidenciaIngresada :: Investigacion -> [Evidencia] --O(E) donde E es la cantidad de envidencias del map
--Propósito: devuelve la evidencia ingresada.
evidenciaIngresada (ConsI mnp mens pqp n) = domM mens

nombresIngresados :: Investigacion -> [Nombre] --O(P) donde P es la cantidad de personas del map
--Propósito: devuelve los nombres de personas ingresadas.
nombresIngresados (ConsI mnp mens pqp n) = domM mnp

casoCerrado :: Investigacion -> Bool -- O(1)
--Propósito: indica si la investigación posee al menos una persona con 5 evidencias en su contra.
casoCerrado (ConsI mnp mens pqp n) = let p = maxPQ pqp in
    if isEmptyPQ pqp
    then False
    else cantEvidencia (maxPQ pqp) >= 5

esSospechoso :: Nombre -> Investigacion -> Bool -- O(log P)
--lookupM = O(log P) donde P es la cantidad de personas del map
--cantEvidencia = O(1)
--Propósito: indica si esa persona tiene al menos una evidencia en su contra.
--Nota: la persona puede no existir.
esSospechoso n' (ConsI mnp mens pqp n) = case (lookupM n' mnp) of
                                    Nothing -> False
                                    Just p  -> cantEvidencia p > 0

posiblesInocentes :: Investigacion -> [Persona] -- O(P log P) por posiblesInocentesEn
--Propósito: devuelve a las personas con cero evidencia en su contra.
posiblesInocentes (ConsI mnp mens pqp n) = posiblesInocentesEn pqp

posiblesInocentesEn :: PriorityQueue Persona -> [Persona] -- O(P*(log P)) donde por cada P en la pq se hace unas operaciones constantes y una operación logaritmica -> O(P log P)
--isEmptyPQ = O(1)
--maxPQ = O(1)
--esInocente = O(1)
--deleteMaxPQ = O(log P) donde P es la cantidad de personas de la priorityqueue
--singularSi = O(1)
posiblesInocentesEn pqp = if isEmptyPQ pqp
                        then []
                        else let p = maxPQ pqp in
                            singularSi p (esInocente p) ++ posiblesInocentesEn (deleteMaxPQ pqp)

esInocente :: Persona -> Bool -- O(1)
esInocente p = cantEvidencia p == 0

singularSi :: Persona -> Bool -> [Persona] --O(1)
singularSi p True  = [p]
singularSi _ False = []

ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion -- O(N*(log P)) donde por cada nombre N en la lista, hace unas operaciones constantes y logaritmicas -> O(N log P)
--lookupM = O(log P) donde P es la cantidad de personas del map
--crearP = O(1)
--assocM = O(log P) donde P es la cantidad de personas del map
--insertPQ = O(log P) donde P es la cantidad de personas de la priority queue
--Propósito: ingresa a personas nuevas a la investigación (mediante sus nombres), sin evidencia en su contra.
--Precondición: las personas no existen en la investigación y no hay nombres repetidos.
ingresarPersonas [] i                          = i
ingresarPersonas (n:ns) (ConsI mnp mens pqp n') = case (lookupM n mnp) of
                            Just p  -> error "La persona ya existe"
                            Nothing -> let p' = crearP n 
                                    i' = ConsI (assocM n p' mnp) mens (insertPQ p' pqp) n' in
                                        ingresarPersonas ns i'


ingresarEvidencia :: Evidencia -> Nombre -> Investigacion -> Investigacion --O(P log P + log E + P') --> O(P log P) dominante
--lookupM = O(log P) donde P es la cantidad de personas del map
--lookupM = O(log E) donde E es la cantidad de evidencias del map
--elem = O(P') donde P' es la cantidad de nombres de personas en la evidencia
--assocM = O(log E) donde E es la cantidad de evidencias del map
--Propósito: asocia una evidencia a una persona dada.
--Precondición: la evidencia aún no está asociada a esa persona.
--Nota: la persona y la evidencia existen, pero NO están asociadas.
ingresarEvidencia e nom (ConsI mnp mens pqp n) = case (lookupM nom mnp) of
                                            Nothing -> error "La persona no existe"
                                            Just p  -> case (lookupM e mens) of
                                                Nothing -> error "La evidencia no existe"
                                                Just ns -> 
                                                        let p' = agregarEvidencia e p in
                                                            if (elem nom ns)
                                                            then error "El nombre ya existe en la lista"
                                                            else ConsI (assocM nom p' mnp) (assocM e (nom:ns) mens) (actualizarPQ p' pqp) n

actualizarPQ :: Persona -> PriorityQueue Persona -> PriorityQueue Persona -- O(P*(log P)) donde en peor caso por cada persona P hago unas operaciones logaritmicas y constantes en la pq -> O(P log P)
--isEmptyPQ = O(1)
--emptyPQ = O(1)
--maxPQ = O(1)
--nombre = O(1)
--insertPQ = O(log P) donde P es la cantidad de personas en la priority queue
--deleteMaxPQ = O(log P) donde P es la cantidad de personas en la priority queue
actualizarPQ p' pqp = if isEmptyPQ pqp
                        then emptyPQ
                        else let p = maxPQ pqp in
                            if (nombre p' == nombre p)
                            then insertPQ p' (deleteMaxPQ pqp)
                            else insertPQ p (actualizarPQ p' (deleteMaxPQ pqp))

--USUARIO DEL TAD 

comenzarConPersonas :: [Nombre] -> Investigacion -- Costo de la operación O(P log P) donde sabemos que por cada P ingresa a la investigación, la investigación va creciendo por cada P
--ingresarPersonas -> O(N log P)
--Propósito: Comienza una investigación con una lista de nombres sin evidencia.
comenzarConPersonas ns = ingresarPersonas ns (comenzarInvestigacion)

todosInocentes :: Investigacion -> Bool -- O(P log P)
--nombresIngresados = O(P) donde P es la cantidad total de personas de la investigacion
--todosInocentesEn = O(P log P)
--Propósito: Indica si las personas en la investigación son todas inocentes.
todosInocentes i = todosInocentesEn (nombresIngresados i) i

todosInocentesEn :: [Nombre] -> Investigacion -> Bool --O(N*(log P)) donde por cada nombre N hace una operación logaritmica en cada iteración, en peor caso son todas las personas de la investigación -> O(P log P)
--esSospechoso = O(log P) donde P es la cantidad de personas de la investigacion
todosInocentesEn [] i     = True
todosInocentesEn (n:ns) i = not (esSospechoso n i) && todosInocentesEn ns i

terminaCerrado :: [(Evidencia, Nombre)] -> Investigacion -> Bool -- O(N*(P log P))
--casoCerrado = O(1)
--agregar = O(N*(P log P))
terminaCerrado ens i = casoCerrado (agregar ens i)

agregar :: [(Evidencia, Nombre)] -> Investigacion -> Investigacion -- O(N*(P log P)) por cada N se hace una operación logaritmica
--ingresarEvidencia = O(P log P)
agregar [] i       = i
agregar (en:ens) i = let (e, n) = en in
    ingresarEvidencia e n (agregar ens i)

{-Dar una posible representación para el tipo Persona, de manera de que se pueda cumplir con el orden dado para cada
operación de la interfaz, pero sin implementarlas.
Puntaje: 0.25-}

data Persona = P Nombre [Evidencia] Int
--Donde Nombre = Nombre de la persona es un string
--[Evidencia] es una lista de elementos de evidencia
--un Int donde corresponde a la cantidad de evidencias

crearP :: Nombre -> Persona O(1)
nombre :: Persona -> Nombre O(1)
evidencia :: Persona -> [Evidencia] O(1)
cantEvidencia :: Persona -> Int O(1)
agregarEvidencia :: Evidencia -> Persona -> Persona O(1)
