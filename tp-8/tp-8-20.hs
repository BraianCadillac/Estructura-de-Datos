{-
Introducción

Se desea modelar un sistema de hechizos encadenados.

Cada hechizo puede:

terminar o aplicar un efecto y continuar con uno o dos hechizos siguientes
-}

data Hechizo = Fin | Efecto Int Bool Hechizo Hechizo
-- Efecto poder esOscuro izq der

--Interpretación:
--Int → poder del efecto
--Bool → indica si es oscuro (True) o luminoso (False)
--izq, der → posibles continuaciones

--Tipo auxiliar
data Dir = Izq | Der

--EJERCICIO 1️
cantidadEfectos :: Hechizo -> Int -- O(E) donde E es la cantidad de efectos del hechizo 
--Propósito: Cuenta la cantidad total de efectos.
cantidadEfectos Fin                 = 0
cantidadEfectos (Efecto p eo hi hd) = 1 + cantidadEfectos hi + cantidadEfectos hd 

--EJERCICIO 2
poderTotal :: Hechizo -> Int -- O(E) donde E es la cantidad de efectos del hechizo
--Propósito: Suma el poder de todos los efectos.
poderTotal Fin                 = 0
poderTotal (Efecto p eo hi hd) = p + poderTotal hi + poderTotal hd

--EJERCICIO 3
oscuridadCamino :: [Dir] -> Hechizo -> Int --O(D) donde D es la cantidad de direcciones de la lista
--Propósito: Cuenta cuántos efectos oscuros hay al recorrer un camino.
--Precondición: el camino existe.
oscuridadCamino [] h                       = 0
oscuridadCamino (d:ds) Fin                 = error "El camino es inválido"
oscuridadCamino (d:ds) (Efecto p eo hi hd) = if (esIzq d)
                                            then contarPoderSi eo + oscuridadCamino ds hi
                                            else contarPoderSi eo + oscuridadCamino ds hd

contarPoderSi :: Bool -> Int -- O(1)
contarPoderSi True  = 1
contarPoderSi False = 0 

esIzq :: Dir -> Bool -- O(1)
esIzq Izq = True
esIzq _   = False

--EJERCICIO 4
existeEfectoFuerte :: Int -> Hechizo -> Bool --O(E) donde E es la cantidad total de efectos del hechizo
--Propósito: Indica si existe algún efecto con poder mayor al dado.
existeEfectoFuerte n Fin                 = False
existeEfectoFuerte n (Efecto p eo hi hd) = p > n || existeEfectoFuerte n hi || existeEfectoFuerte n hd

--EJERCICIO 5
nivelN :: Int -> Hechizo -> [(Int,Bool)] -- O(E) donde E es la cantidad de efectos del hechizo
--Propósito: Devuelve los efectos del nivel n como pares:
nivelN 0 Fin                 = []
nivelN 0 (Efecto p eo hi hd) = [(p, eo)]
nivelN n Fin                 = []
nivelN n (Efecto p eo hi hd) = nivelN (n-1) hi ++ nivelN (n-1) hd

--EJERCICIO 6
podarOscuros :: Hechizo -> Hechizo --O(E) donde E en peor caso es la cantidad de efectos de todo el hechizo
--Propósito: Elimina todos los subárboles cuya raíz sea un efecto oscuro.
podarOscuros Fin                 = Fin
podarOscuros (Efecto p eo hi hd) = if eo
                                then Fin
                                else Efecto p eo (podarOscuros hi) (podarOscuros hd)

--EJERCICIO 7
hayCaminoLuminoso :: Hechizo -> Bool --O(E) donde E es la cantidad de Efectos del hechizo
--Propósito: Indica si existe un camino desde la raíz hasta Fin tal que todos los efectos sean luminosos.
hayCaminoLuminoso Fin                 = True
hayCaminoLuminoso (Efecto p eo hi hd) = not eo && (hayCaminoLuminoso hi || hayCaminoLuminoso hd)

--EJERCICIO 8
caminoMasPoderoso :: Hechizo -> [Dir]
--Propósito: Devuelve el camino que acumula mayor poder total. Si hay empate, cualquiera.
caminoMasPoderoso h = let (ds, pt) = caminoMasPoderosoEn h in
    ds

caminoMasPoderosoEn :: Hechizo -> ([Dir], Int) --O(E) donde E es la cantidad de efectos del hechizo
caminoMasPoderosoEn Fin                 = ([], 0)
caminoMasPoderosoEn (Efecto p eo hi hd) = case (caminoMasPoderosoEn hi) of
                                        (dsi, pti) -> case (caminoMasPoderosoEn hd) of
                                                    (dsd, ptd) -> if pti > ptd
                                                                then (Izq:dsi, p+pti)
                                                                else (Der:dsd, p+ptd)

--EJERCICIO 9
maximoConRestriccion :: Hechizo -> Int
--Propósito: Devuelve el máximo poder acumulado en un camino tal que: 👉 no puede haber dos efectos oscuros consecutivos
maximoConRestriccion Fin                 = 0
maximoConRestriccion (Efecto p eo hi hd) = if eo
                                            then p + max (maximoConRestriccionEn 1 hi) (maximoConRestriccionEn 1 hd)
                                            else p + max (maximoConRestriccionEn 2 hi) (maximoConRestriccionEn 2 hd)

maximoConRestriccionEn :: Int -> Hechizo -> Int
maximoConRestriccionEn 0 h                   = error "Hay dos efectos oscuros consecutivos"
maximoConRestriccionEn n Fin                 = 0
maximoConRestriccionEn n (Efecto p eo hi hd) = if eo
                                            then p + max (maximoConRestriccionEn (n-1) hi) (maximoConRestriccionEn (n-1) hd)
                                            else p + max (maximoConRestriccionEn n hi) (maximoConRestriccionEn n hd)

--EJERCICIO 10
--Propósito: Devuelve el camino más largo desde la raíz hasta Fin cumpliendo:
--la cantidad total de efectos oscuros en el camino ≤ 2
--Si no existe, devolver [].

caminoValidoMasLargo :: Hechizo -> [Dir]
caminoValidoMasLargo h = camino (caminoMasLargo h)

caminoMasLargo :: Hechizo -> (Maybe [Dir], Int)
caminoMasLargo Fin                 = 
caminoMasLargo (Efecto p eo hi hd) = if eo
                                    then case (caminoMasLargo hi, caminoMasLargo hd) of
                                        ((Nothing, eoti), (Nothing, eotd))   -> (Nothing, 1)
                                        ((Just dsi, eoti), (Nothing, eotd))  -> if (1 + eoti) <= 2
                                                                             then (Izq:dsi, 1 + eoti)
                                                                             else (Nothing, 1 + eotd)
                                        ((Nothing, eoti), ( Just dsd, eotd)) -> if (1 + eotd) <= 2
                                                                             then (Der:dsd, 1 + eotd)
                                                                             else (Nothing, 1 + eoti)
                                        ((Just dsi, eoti), (Just dsd, eotd))          -> if (length dsi > length dsd && (1 + eoti) <= 2)
                                                                             then (Just Izq:dsi, 1 + eoti)
                                                                             else if (length dsd > length dsi && (1 + eotd) <= 2)
                                                                                 then (Just Der:dsi, 1 + eotd)
                                                                                 else (Nothing, 1 + eoti + eotd)
                                    else case (caminoMasLargo hi, caminoMasLargo hd) of
                                        ((Nothing, eoti), (Nothing, eotd))   -> (Nothing, 0)
                                        ((Just dsi, eoti), (Nothing, eotd))  -> if eoti <= 2
                                                                             then (Just Izq:dsi, eoti)
                                                                             else (Nothing, eotd)
                                        ((Nothing, eoti), (Just dsd, eotd))  -> if eotd <= 2
                                                                             then (Just Der:dsd, eotd)
                                                                             else (Nothing, eoti)
                                        ((Just dsi, eoti), (Just dsd, eotd)) -> if (length dsi > length dsd && eoti <= 2)
                                                                            then (Just Izq:dsi, eoti)
                                                                            else if (length dsd > length dsi && eotd <= 2)
                                                                                then (Just Der:dsi, eotd)
                                                                                else (Nothing, eoti + eotd)

camino :: (Maybe [Dir], Int) -> [Dir]
camino (Nothing, _) = []
camino (Just ds, _) = ds

