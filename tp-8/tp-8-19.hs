{-
Introducción

Se desea modelar un sistema de torres mágicas.

Cada torre está formada por salas conectadas en forma de árbol binario.
En cada sala hay:

una cantidad de energía
una cantidad de enemigos

Definimos:
-}

data Torre = Vacio
           | Sala Int Int Torre Torre
-- Sala energia enemigos izquierda derecha

data Dir = Izq | Der

{-
Interpretación
Vacio representa ausencia de sala / fin del camino.
Sala e m izq der representa una sala con:
e energía
m enemigos
-}

--EJERCICIO 1
energiaTotal :: Torre -> Int --O(S) donde S es la cantidad total de salas de la torre
--Propósito: Devuelve la suma total de energía de todas las salas.
energiaTotal Vacio            = 0
energiaTotal (Sala e m si sd) = e + energiaTotal si + energiaTotal sd

--EJERCICIO 2
cantidadSalas :: Torre -> Int -- O(S) donde S es la cantidad total de salas de la torre
--Propósito: Cuenta la cantidad total de salas.
cantidadSalas Vacio            = 0
cantidadSalas (Sala e m si sd) = 1 + cantidadSalas si + cantidadSalas sd

--EJERCICIO 3
enemigosCamino :: [Dir] -> Torre -> Int --O(D) donde D es la cantidad de direcciones de la lista
--Propósito: Dado un camino desde la raíz, devuelve la cantidad total de enemigos encontrados.
--Precondición: el camino existe.
enemigosCamino [] t                    = 0
enemigosCamino (d:ds) Vacio            = error "El camino es inválido"
enemigosCamino (d:ds) (Sala e m si sd) = if (esIzq d)
                                        then m + enemigosCamino ds si
                                        else m + enemigosCamino ds sd

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False


--EJERCICIO 4
haySalaSinEnemigos :: Torre -> Bool -- O(S) donde S es la cantidad total de salas de la torre
--Propósito: Indica si existe al menos una sala con 0 enemigos.
haySalaSinEnemigos Vacio            = False
haySalaSinEnemigos (Sala e m si sd) = m == 0 || haySalaSinEnemigos si || haySalaSinEnemigos sd

--EJERCICIO 5
nivelN :: Int -> Torre -> [Int] --O(S) donde en peor caso S es la cantidad de salas de toda la torre
Propósito:
--Propósito: Devuelve la lista de energías de las salas en el nivel n.
nivelN 0 Vacio            = []
nivelN 0 (Sala e m si sd) = [e]
nivelN n Vacio            = []
nivelN n (Sala e m si sd) = nivelN (n-1) si ++ nivelN (n-1) sd

--EJERCICIO 6
podarConMuchosEnemigos :: Int -> Torre -> Torre --O(S) donde S en peor caso es la cantidad de salas de la torre
--Propósito: Elimina (reemplaza por Vacio) todos los subárboles cuya raíz tenga más enemigos que el número dado.
podarConMuchosEnemigos ce Vacio            = Vacio
podarConMuchosEnemigos ce (Sala e m si sd) = if (m > ce)
                                            then Vacio
                                            else Sala e m (podarConMuchosEnemigos ce si) (podarConMuchosEnemigos ce sd)


--EJERCICIO 8 (INTERESANTE)
existeCaminoSeguro :: Torre -> Bool -- O(S) donde S es la cantidad total de salas de la torre
--Propósito: Indica si existe un camino desde la raíz hasta Vacio tal que:
-- 👉 todas las salas del camino tengan enemigos = 0
existeCaminoSeguro Vacio            = True
existeCaminoSeguro (Sala e m si sd) = m == 0 && (existeCaminoSeguro si || existeCaminoSeguro sd)

--EJERCICIO 9 (DESAFÍO MEDIO)
longitudCaminoMasLargo :: Torre -> Int 
--Propósito: Devuelve la longitud (cantidad de pasos) del camino más largo desde la raíz hasta Vacio.
longitudCaminoMasLargo Vacio            = 0
longitudCaminoMasLargo (Sala e m si sd) = 1 + max (longitudCaminoMasLargo si) (longitudCaminoMasLargo sd)

--EJERCICIO 10 (DESAFÍO FINAL PERO MÁS TRANQUILO)
energiaMaximaSinRiesgo :: Torre -> Int
Propósito:
--Propósito: Devuelve la máxima energía que se puede recolectar en un camino desde la raíz hasta Vacio cumpliendo:
-- 👉 no se puede pasar por dos salas consecutivas con enemigos > 0
energiaMaximaSinRiesgo t = energiaMaximaSinRiesgoAlPaso 2 t

energiaMaximaSinRiesgoAlPaso :: Int -> Torre -> Int
energiaMaximaSinRiesgoAlPaso 0 t                = error "Diste dos pasos consecutivos con enemigos"
energiaMaximaSinRiesgoAlPaso p Vacio            = 0
energiaMaximaSinRiesgoAlPaso p (Sala e m si sd) = if (m > 0)
                                                then e + max (energiaMaximaSinRiesgoAlPaso (p-1) si) (energiaMaximaSinRiesgoAlPaso (p-1) sd)
                                                else e + max (energiaMaximaSinRiesgoAlPaso p si) (energiaMaximaSinRiesgoAlPaso p sd)

