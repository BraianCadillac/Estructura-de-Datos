PARCIAL - Recursión Estructural Pura

Introducción
Se desea modelar una red de túneles subterráneos. Cada túnel puede bifurcarse en dos caminos,
y en cada sala puede haber tesoros y guardianes.

Definimos los siguientes tipos:

data Tunel = Fin
           | Sala Int Int Tunel Tunel

-- Sala tesoros guardianes caminoIzq caminoDer

Interpretación:
- Fin representa el final de un camino.
- Sala t g izq der representa una sala con:
    t = cantidad de tesoros
    g = cantidad de guardianes
    dos caminos posibles.

--------------------------------------------------
EJERCICIO 1
salasTotales :: Tunel -> Int

Propósito:
Devuelve la cantidad total de salas del túnel.

salasTotales :: Tunel -> Int -- O(S) donde S es la cantidad total de salas del tunel
salasTotales Fin              = 0
salasTotales (Sala _ _ ti td) = 1 + salasTotales ti + salasTotales td

--------------------------------------------------
EJERCICIO 2
tesorosTotales :: Tunel -> Int -- O(S) donde S es la cantidad total de salas del tunel
salasTotales Fin              = 0
salasTotales (Sala t _ ti td) = t + tesorosTotales ti + tesorosTotales td

Propósito:
Devuelve la suma de todos los tesoros del túnel.

--------------------------------------------------
EJERCICIO 3
guardianesDelCamino :: [Dir] -> Tunel -> Int --O(D) donde D es la cantidad de direcciones de la lista
guardianesDelCamino [] Fin                  = 0
guardianesDelCamino [] (Sala _ g ti td)     = g
guardianesDelCamino (d:ds) Fin              = error "Camino inválido"
guardianesDelCamino (d:ds) (Sala _ g ti td) = if (esIzq d) 
                                            then g + guardianesDelCamino ds ti 
                                            else g + guardianesDelCamino ds td

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False

data Dir = Izq | Der

Propósito:
Dado un camino, devuelve la cantidad total de guardianes encontrados
recorriendo ese camino desde la raíz.

Precondición:
El camino existe.

Ejemplo:
[Izq,Der] significa:
ir izquierda, luego derecha.

--------------------------------------------------
EJERCICIO 4
caminoMasTesoros :: Tunel -> [Dir] -- O(los S) donde S es la cantidad de salas del tunel
caminoMasTesoros t = let (ds, ts) = caminoMasTesoros' t in
    ds

caminoMasTesoros' :: Tunel -> ([Dir], Int)
caminoMasTesoros' Fin              = ([], 0)
caminoMasTesoros' (Sala t g ti td) = case (caminoMasTesoros' ti) of
                                    (dsi, tsi) -> case (caminoMasTesoros' td) of
                                            (dsd, tsd) -> if (tsi > tsd)
                                                        then (Izq:dsi, t + tsi)
                                                        else (Der:dsd, t + tsd)

Propósito:
Devuelve el camino desde la raíz hasta un Fin que acumula mayor cantidad de tesoros.

Si hay empate, cualquiera es válido.

--------------------------------------------------
EJERCICIO 5
podarPeligrosas :: Int -> Tunel -> Tunel --O(S) donde S en peor caso es la cantidad total de salas del tunel
podarPeligrosas cg Fin              = Fin
podarPeligrosas cg (Sala t g ti td) = if (g > cg)
                                    then Fin
                                    else Sala t g (podarPeligrosas cg ti) (podarPeligrosas cg td)

Propósito:
Elimina (reemplaza por Fin) todos los subárboles cuya raíz tenga
más guardianes que el número dado.

Ejemplo:
podarPeligrosas 3 elimina salas con guardianes > 3.

--------------------------------------------------
EJERCICIO 6
salasNivelN :: Int -> Tunel -> [(Int,Int)] --O(S) donde en peor caso S es la cantidad total de todas las salas del tunel
salasNivelN n Fin              = []
salasNivelN 0 (Sala t g _ _)   = [(t, g)]
salasNivelN n (Sala t g ti td) = salasNivelN (n-1) ti ++ salasNivelN (n-1) td

Propósito:
Devuelve todas las salas del nivel n como pares:

(tesoros, guardianes)

Nivel 0 = raíz
Nivel 1 = hijos de raíz
etc.

--------------------------------------------------
EJERCICIO 7
esBalanceado :: Tunel -> Bool --O(S log S) donde por cada sala S hace una operación dos operaciones log S -> O(S log S)
esBalanceado Fin              = True
esBalanceado (Sala t g ti td) = abs (alturaT ti - alturaT td) <= 1 && esBalanceado ti && esBalanceado td

alturaT :: Tunel -> Int
alturaT Fin              = 0
alturaT (Sala t g ti td) = 1 + max (alturaT ti) (alturaT td) 


Propósito:
Indica si para toda sala, la diferencia entre cantidad de salas
del subárbol izquierdo y derecho es como máximo 1.

--------------------------------------------------
EJERCICIO 8 (DESAFIANTE)
rutaSegura :: Tunel -> [Dir]
rutaSegura t = ruta (rutaSeguraAux t)

rutaSeguraAux :: Tunel -> Maybe [Dir]
rutaSeguraAux Fin = Just []
rutaSeguraAux (Sala _ g ti td) = if g /= 0
                                then Nothing
                                else
                                    case (rutaSeguraAux ti, rutaSeguraAux td) of
                                        (Nothing, Nothing) -> Just []
                                        (Just ci, Nothing) -> Just (Izq : ci)
                                        (Nothing, Just cd) -> Just (Der : cd)
                                        (Just ci, Just cd) ->
                                            if length ci >= length cd
                                            then Just (Izq : ci)
                                            else Just (Der : cd)

ruta :: Maybe [Dir] -> [Dir]
ruta Nothing   = []
ruta (Just ds) = ds

Propósito:
Devuelve el camino más largo posible desde la raíz hasta Fin
pasando solamente por salas con guardianes = 0.

Si no existe camino seguro desde la raíz, devuelve [].

--------------------------------------------------

