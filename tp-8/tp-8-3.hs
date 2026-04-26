{-
Ejercicios
Recordatorio: De existir, agregue las precondiciones en las funciones solicitadas. ¡No deje de dividir en subtareas! Y no olvide
además incluir propósito y precondiciones de las funciones auxiliares que necesite programar.
1) Implementar las siguientes funciones como usuario del TAD Organizador, establecer su eficiencia y justificarla:

a) programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
programaron juntas.

b) esUnGranHacker :: Organizador -> Persona -> Bool
Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
-}

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum -- Se realizan dos llamadas a programasDe de costo O(log P). Luego se aplica intersection sobre dos conjuntos de tamaño C, cuyo costo es O(C log C). Por lo tanto, el costo total es O(log P + C log C).
--programasDe = O(log P)
programasEnComun p1 p2 o = intersection (programasDe p1 o) (programasDe p2 o)

esUnGranHacker :: Organizador -> Persona -> Bool -- O(C* (log P + log C))
--todosLosProgramas = O(C) donde C es la cantidad de códigos del organizador
esUnGranHacker o p = esUnGranHackerEn (todosLosProgramas o) o p

esUnGranHackerEn :: [Checksum] -> Organizador -> Persona -> Bool -- O(C* (log P + log C))
--belongs = O(log P) siendo P la cantidad de personas del set
--autoresDe = O(log C) donde C es la cantidad de programas del organizador
esUnGranHackerEn [] o p     = True
esUnGranHackerEn (c:cs) o p = belongs p (autoresDe o c) && esUnGranHackerEn cs o p

{-
b) Implementar el TAD Organizador suponiendo el siguiente tipo de representación:

a) Escribir los invariantes de representación para poder crear elementos válidos del TAD.
-}


data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
    {-
    INV REPR: En (MkO mcsp mpsc) se cumple que:
            * Para cada Checksum c en mcsp, cada Persona p del conjunto asociado a c
                aparece como clave en mpsc y además c pertenece al conjunto asociado a p
            * Para cada Persona p en mpsc, cada Checksum c del conjunto asociado a p
                aparece como clave en mcsp y además p pertenece al conjunto asociado a c
            * No hay conjuntos vacíos como valores en mcsp
    -}

--b)

nuevo :: Organizador -- O(1)
--Propósito: Un organizador vacío.
--Eficiencia: O(1)
nuevo = MkO emptyM emptyM


agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador -- O(P*(log P + log C))
--isEmptyS = O(1)
--lookupM = O(log C) siendo C la cantidad de programas en el map
--assocM = O(log C) siendo C la cantidad de programas en el map
--set2list = O(P) siendo P todas las personas del set
-- agregarProgramasA =
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (MkO mcsp mpsc mc) c sp = if isEmptyS sp
                                        then error "No hay personas en el set"
                                        else case (lookupM c mcsp) of
                                            Nothing -> MkO (assocM c sp mcsp) (agregarProgramasA (set2list sp) c mpsc) (programaConMas mcsp mc c sp)
                                            Just sp -> error "El programa fue usado previamente en el organizador"

programaConMas :: Map Checksum (Set Persona) -> Maybe Checksum -> Checksum -> Set Persona -> Maybe Checksum -- O(log C)
--lookupM = O(log C) siendo C la cantidad de programas en el map
programaConMas _ Nothing c sp     = Just c
programaConMas mcsp (Just c) ch spn = case (lookupM c mcsp) of
                                    Nothing -> error "El programa no existe"
                                    Just sp -> if (sizeS spn >= sizeS sp)
                                                then Just ch
                                                else Just c

agregarProgramasA :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum) -- costo de la operación = O(P*(log P + log C)) por cada P se hace 3 operaciones logaritmicas, dos operaciones O(log P) y una O(log C)
--lookupM = O(log P) siendo P la cantidad de personas en el map
--assocM = O (log P) siendo P la cantidad de personas en el map
--addS = O(log C) siendo C la cantidad de programas en el set
agregarProgramasA [] c mpsc     = mpsc
agregarProgramasA (p:ps) c mpsc = case (lookupM p mpsc) of
                                Nothing -> assocM p (addS c emptyS) (agregarProgramasA ps c mpsc)
                                Just sc -> assocM p (addS c sc) (agregarProgramasA ps c mpsc)


todosLosProgramas :: Organizador -> [Checksum] --O(C) donde C es la cantidad de programas en el organizador
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MkO mcsp mpsc) = domM mcsp 



autoresDe :: Organizador -> Checksum -> Set Persona -- O(log C)
--lookupM = O(log C) donde C es la cantidad de programas del organizador
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MkO mcsp mpsc) c = case (lookupM c mcsp) of
                            Nothing -> error "No corresponde a un programa del organizador"
                            Just sp -> sp

programasDe :: Organizador -> Persona -> Set Checksum -- O(log P)
--lookupM = O(log P) siendo P la cantidad de personas en el map
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MkO mcsp mpsc) p = case (lookupM p mpsc) of
                                Nothing -> error "La persona no existe"
                                Just sc -> sc



programaronJuntas :: Organizador -> Persona -> Persona -> Bool -- Costo de la operación = O(log P + C log C)
--lookupM = O(log P) siendo P la cantidad de personas del map
--sizeS = O(1)
--intersection = O(C log C) donde por cada C hace una operación O(log C) en sc'
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas.
programaronJuntas (MkO mcsp mpsc) p1 p2 = if p1 == p2
                                            then error "Las personas son iguales"
                                            else case (lookupM p1 mpsc) of
                                                Nothing -> error "La persona no existe"
                                                Just sc -> case (lookupM p2 mpsc) of
                                                        Nothing  -> error "La persona no existe"
                                                        Just sc' -> sizeS (intersection sc sc') > 0

nroProgramasDePersona :: Organizador -> Persona -> Int -- Costo de la operación = O(log P)
--lookupM = O(log P) siendo P la cantidad de personas en el map
--sizeS = O(1)
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
nroProgramasDePersona (MkO mcsp mpsc) p = case (lookupM p mpsc) of
                                        Nothing -> error "La persona no existe"
                                        Just sc -> sizeS sc


{-
c) Implementar una variante del TAD Organizador suponiendo que en la interfaz del TAD Organizador se agrega una nueva
operación:
elMayorPrograma :: Organizador -> Maybe Checksum
Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
Nothing si no puede devolver un programa.
Eficiencia: O(1) en peor caso.
Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching.
-}

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) (Maybe Checksum)
    {-
    INV REPR: En (MkO mcsp mpsc mc)
            *En mc, para un (Just c), c es el programa con más autores en el organizador, Nothing sino hay programas 
    -}

elMayorPrograma :: Organizador -> Maybe Checksum -- O(1)
elMayorPrograma (MkO mcsp mpsc mc) = mc