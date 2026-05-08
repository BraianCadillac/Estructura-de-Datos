{-
Vamos a recorrer árboles de celdas Gobstones usando una lista de direcciones, que marcarán un camino dentro
de un árbol.
-}

--Nota: El orden de las bolitas en el constructor ConsCelda es primero verdes y luego rojas.

data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

data Dir = Izq | Der

data Celda = ConsCelda Int Int

--Implementar
--Nota: Todas las funciones que reciben un camino por parámetro tienen la precondición de que el camino existe en el árbol, salvo la primera, que chequea si un camino existe o no.
existeCamino :: [Dir] -> Arbol Celda -> Bool
--PROP:Dado un camino y un árbol, indica si ese camino existe en el árbol. Un camino existe si puedo completarlo sin que se termine el árbol.
existeCamino [] Vacio                = False
existeCamino [] (Nodo c aci acd)     = True  
existeCamino (d:ds) Vacio            = False
existeCamino (d:ds) (Nodo c aci acd) = if (esIzq d)
                                    then existeCamino ds aci
                                    else existeCamino ds acd

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False

rojasDeCelda :: [Dir] -> Arbol Celda -> Int
--PROP: Dado un camino y un arbol, retorna la cantidad de bolitas rojas que posee la celda al final del camino.
rojasDeCelda [] Vacio                = error "Camino inválido"
rojasDeCelda [] (Nodo c aci acd)     = cantidadDeRojas c
rojasDeCelda (d:ds) Vacio            = error "Camino inválido"
rojasDeCelda (d:ds) (Nodo c aci acd) = if (esIzq d)
                                    then rojasDeCelda ds aci
                                    else rojasDeCelda ds acd

cantidadDeRojas :: Celda -> Int
cantidadDeRojas (ConsCelda v r) = r

celdaConMasRojas :: Arbol Celda -> Celda
--PROP: Dado un árbol de celdas retorna la celda que tenga más bolitas rojas.
celdaConMasRojas Vacio            = ConsCelda 0 0
celdaConMasRojas (Nodo c aci acd) = celdaConMasRojasDe c (celdaConMasRojasDe (celdaConMasRojas aci) (celdaConMasRojas acd))

celdaConMasRojasDe :: Celda -> Celda -> Celda
celdaConMasRojasDe c1 c2 = if (cantidadDeRojas c1 > cantidadDeRojas c2)
                            then c1
                            else c2

vaciarCeldas :: [[Dir]] -> Arbol Celda -> Arbol Celda
--PROP: Dada una lista de caminos vacía las celdas que se encuentren al final de dichos caminos.
vaciarCeldas [] ac       = ac
vaciarCeldas (ds:dss) ac = vaciarCelda ds (vaciarCeldas dss ac)

vaciarCelda :: [Dir] -> Arbol Celda -> Arbol Celda
vaciarCelda [] Vacio                = error "Cámino inválido"
vaciarCelda [] (Nodo c aci acd)     = Nodo (vaciar c) aci acd
vaciarCelda (d:ds) Vacio            = error "Camino inválido"
vaciarCelda (d:ds) (Nodo c aci acd) = if (esIzq d)
                                    then Nodo c (vaciarCelda ds aci) acd
                                    else Nodo c aci (vaciarCelda ds acd)

vaciar :: Celda -> Celda
vaciar (ConsCelda v r) = ConsCelda 0 0

caminoMasLargo :: Arbol Celda -> [Dir]
-- PROP: devuelve el camino más largo desde la raíz hasta alguna hoja.
caminoMasLargo Vacio                = []
caminoMasLargo (Nodo c aci acd)     = if (esHoja aci acd)
                                    then []
                                    else if (length (caminoMasLargo aci) > length(caminoMasLargo acd))
                                        then Izq : caminoMasLargo aci
                                        else Der : caminoMasLargo acd

esHoja :: Arbol Celda -> Arbol Celda -> Bool 
esHoja Vacio Vacio = True
esHoja _ _         = False