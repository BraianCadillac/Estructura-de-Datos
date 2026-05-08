PARCIAL – Recursión Estructural Pura II
(Nivel: similar / levemente superior al anterior)

Introducción

Se desea modelar un sistema de cavernas encantadas.
Cada caverna está compuesta por salas conectadas en forma de árbol binario.
En cada sala hay:

- una cantidad de monedas
- una cantidad de monstruos

Definimos:

data Caverna = Nada
             | Sala Int Int Caverna Caverna

-- Sala monedas monstruos izquierda derecha

Además:

data Dir = Izq | Der

Interpretación:

- Nada representa ausencia de sala / fin del camino.
- Sala m k izq der representa una sala con:
      m monedas
      k monstruos

--------------------------------------------------
EJERCICIO 1

cantidadSalas :: Caverna -> Int --O(S) donde S es la cantidad total de salas de la caverna
cantidadSalas Nada               = 0
cantidadSalas (Sala ms ks ci cd) = 1 + cantidadSalas ci + cantidadSalas cd

Propósito:
Devuelve la cantidad total de salas de la caverna.

--------------------------------------------------
EJERCICIO 2

monedasTotales :: Caverna -> Int --O(S) donde S es la cantidad total de salas de la caverna
monedasTotales Nada               = 0
monedasTotales (Sala ms ks ci cd) = ms + monedasTotales ci + monedasTotales cd

Propósito:
Devuelve la suma total de monedas de todas las salas.

--------------------------------------------------
EJERCICIO 3

monstruosCamino :: [Dir] -> Caverna -> Int
monstruosCamino [] Nada                   = 0
monstruosCamino (d:ds) Nada               = error "Camino inválido"
monstruosCamino [] (Sala ms ks ci cd)     = 0
monstruosCamino (d:ds) (Sala ms ks ci cd) = if (esIzq d)
                                            then ks + monstruosCamino ds ci
                                            else ks + monstruosCamino ds cd

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False

Propósito:
Dado un camino, devuelve la cantidad total de monstruos
encontrados desde la raíz hasta el final del camino.

Precondición:
El camino existe.

--------------------------------------------------
EJERCICIO 4

caminoMasMonedas :: Caverna -> [Dir] -- O(log S) por caminoMasMonedas'
caminoMasMonedas c = let (ds, ms) = caminoMasMonedas' c in
    ds

caminoMasMonedas' :: Caverna -> ([Dir], Int) --O(log S) donde S es la cantidad de salas de la caverna
caminoMasMonedas' Nada               = ([], 0)
caminoMasMonedas' (Sala ms ks ci cd) = case (caminoMasMonedas' ci) of
                                    (dsi, msi) -> case (caminoMasMonedas' cd) of
                                                (dsd, msd) -> if (msi > msd)
                                                            then (Izq:dsi, ms+msi)
                                                            else (Der:dsd, ms+msd)

Propósito:
Devuelve el camino desde la raíz hasta algún Nada
que acumule mayor cantidad de monedas.

Si hay empate, cualquiera es válido.

--------------------------------------------------
EJERCICIO 5

cerrarPeligrosas :: Int -> Caverna -> Caverna --O(S) donde en peor caso S es la cantidad total de salas de la caverna
cerrarPeligrosas cm Nada               = Nada
cerrarPeligrosas cm (Sala ms ks ci cd) = if (ks > cm)
                                        then Nada
                                        else Sala ms ks (cerrarPeligrosas cm ci) (cerrarPeligrosas cm cd)

Propósito:
Reemplaza por Nada todo subárbol cuya raíz tenga
más monstruos que el valor dado.

--------------------------------------------------
EJERCICIO 6

salasNivelN :: Int -> Caverna -> [(Int,Int)] --O(S) en peor caso donde S es la cantidad total de todas las salas de la caverna
salasNivelN n Nada                = []
salasNivelN 0 (Sala ms ks ci cd) = [(ms, ks)]
salasNivelN n (Sala ms ks ci cd) = salasNivelN (n-1) ci ++ salasNivelN (n-1) cd

Propósito:
Devuelve las salas del nivel n como pares:

(monedas, monstruos)

Nivel 0 = raíz.

--------------------------------------------------
EJERCICIO 7

sinMonstruos :: Caverna -> Bool --O(D^2)
sinMonstruos Fin                = True
sinMonstruos (Sala ms ks ci cd) = ks == 0 && (sinMonstruos ci || sinMonstruos cd)

Propósito:
Indica si existe al menos un camino completo
desde la raíz hasta Nada pasando solamente
por salas con 0 monstruos.

--------------------------------------------------
EJERCICIO 8

caminoMasSeguro :: Caverna -> [Dir]
caminoMasSeguro c = caminoSeguro (caminoAuxiliar c) 

caminoAuxiliar :: Caverna -> (Maybe [Dir], Int)
caminoAuxiliar Nada                = (Nothing, 0)
caminoAuxiliar (Sala ms ks ci cd) =  case (caminoAuxiliar ci, caminoAuxiliar cd) of
                                        ((Nothing, msi), (Nothing, msd))   -> (Just [], 0)
                                        ((Just dsi, msi), (Nothing, msd))  -> (Just (Izq : dsi), ks+msi)
                                        ((Nothing, msi), (Just dsd, msd))  -> (Just (Der : dsd), ks+msd)
                                        ((Just dsi, msi), (Just dsd, msd)) -> if (ks + msi) <= 3 && (ks + msd) <= 3
                                                                               then if length dsi >= length dsd
                                                                                    then (Just (Izq:dsi), ks+msi)
                                                                                    else (Just (Der:dsd), ks+msd)

                                                                               else if (ks + msi) <= 3
                                                                                    then (Just (Izq:dsi), ks+msi)

                                                                                     else if (ks + msd) <= 3
                                                                                          then (Just (Der:dsd), ks+msd)
                                                                                          else (Nothing,0)

caminoSeguro :: (Maybe [Dir], Int) -> [Dir]
caminoSeguro (Nothing, _) = []
caminoSeguro (Just ds, _) = ds

Propósito:
Devuelve el camino más largo desde la raíz hasta Nada
tal que la suma total de monstruos del camino sea <= 3.

Si no existe ninguno, devuelve [].

--------------------------------------------------
EJERCICIO 9 (DESAFIANTE)

recolectarMaximo :: Caverna -> Int
recolectarMaximo c = recolectarMaximoEn 2 c

recolectarMaximoEn :: Int -> Caverna -> Int
recolectarMaximoEn 0 c                  = error "Pasaste por dos salas consecutivas con monstruos"
recolectarMaximoEn n Nada                = 0
recolectarMaximoEn n (Sala ms ks ci cd) = if ks > 0
                                        then case (recolectarMaximoEn (n-1) ci, recolectarMaximoEn (n-1) cd) of
                                                (msi, msd) -> if msi > msd
                                                            then ms + msi
                                                            else ms + msd
                                        else case (recolectarMaximoEn n ci, recolectarMaximoEn n cd) of
                                                (msi, msd) -> if msi > msd
                                                            then ms + msi
                                                            else ms + msd


Propósito:
Devuelve la máxima cantidad de monedas que se puede juntar
recorriendo un único camino desde la raíz hasta Nada,
con la restricción de que no se puede pasar por dos salas
consecutivas que tengan monstruos > 0.

--------------------------------------------------
EJERCICIO 10 (MUY DESAFIANTE)

filtrarCaminos :: Int -> Caverna -> Caverna
filtrarCaminos n c = filtrarCaminosEn (todosLosCaminos c) n c

filtrarCaminosEn :: [[Dir]] -> Int -> Caverna -> Caverna
filtrarCaminos [] n c       = 
filtrarCaminos (ds:dss) n c = (filtrarCaminosEn dss n c)

Propósito:
Elimina todos los caminos que NO lleguen a juntar
al menos n monedas desde la raíz hasta Nada.

Las ramas que no cumplen deben reemplazarse por Nada.

--------------------------------------------------

Reglas:

- Resolver solo con recursión estructural.
- Sin map
- Sin filter
- Sin fold
- Sin listas por comprensión
- Se permiten auxiliares recursivas.
- No usar acumuladores imperativos.