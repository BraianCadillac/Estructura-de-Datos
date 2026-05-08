--EJERCICIO 1: Usuario

personasOrdenadas :: RondaMate -> [Persona] -- O(N*(N + N + N log N)) por cada N se hacen 2 operaciones lineales y una logaritmica -> O(N*(N log N)) -> O(N^2 log N)
--cantidadDePersonas : O(N)
--quienMenosTomó : O(N)
--quitar = O(N x log N)
--Propósito: Dada una ronda de mate, computa la lista de las personas ordenadas de menor a mayor según la cantidad de mates que tomaron.
--Precondición: Todas las personas de la ronda tomaron al menos un mate.
personasOrdenadas rm = if (cantidadDePersonas rm) == 0
                        then []
                        else let (p, m) = quienMenosTomó rm in
                            p : personasOrdenadas (quitar p rm)

{-
Considerando la siguiente representación:

a) Indique invariantes de representación.
b) Implemente las funciones de la interfaz de RondaMate respetando las restricciones de eficiencia pedidas. Justifique en
cada caso por qué se obtiene la eficiencia buscada.
-}

data RondaMate = RM (Ronda Persona) (MultiSet Persona) Int
        {- INV REPR:
                En (RM rp mp n) se cumple:
                        * n > 0
                        * Toda persona que aparece en rp aparece en mp.
                        * Toda persona que aparece en mp aparece en rp.
                        * Ninguna persona aparece repetida en rp.
                        * Para toda persona p en rp, ocurrencesMS p mp indica la cantidad de mates que tomó p.
        -}

nuevaRondaMate :: Int -> [Persona] -> RondaMate --O(P + P log P) -> O(P log P)
--nuevaRonda = O(P) donde P es la cantidad de personas de la ronda
--agregarMS = O(P log P)
--Propósito: Dados un número n y una lista de personas, construye una nueva ronda de mate con las personas de la lista y una cantidad máxima de n mates.
--Precondiciones: La lista no está vacía y el número es mayor que 0.
--Eficiencia: O(N)
nuevaRondaMate _ [] = error "La lista de personas está vacía"
nuevaRondaMate 0 _  = error "No hay mates para cebar"
nuevaRondaMate n ps = let rp = nuevaRonda ps in
    RM rp (agregarMS ps) n

agregarMS :: [Persona] -> MultiSet Persona --O(P*(log P)) donde P es la cantidad de personas de la lista, por cada P hace una operación logaritmica, costo de la operación -> O(P log P)
--addMS = O(log P) donde P es la cantidad de personas del multiset
agregarMS []     = emptyMS
agregarMS (p:ps) = addMS p (agregarMS ps)

cebar :: RondaMate -> RondaMate
--Propósito: Dada una ronda de mate, computa la ronda de mate resultante de que se cebe un mate. El cebado consiste en que se sirva agua al mate y se pase a la siguiente persona en la ronda.
--Precondiciones: Queda agua para un mate.
cebar (RM rp mp n) = 