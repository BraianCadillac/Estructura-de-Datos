

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

{-
Definir las siguientes funciones:
cantidadDeCapas :: Pizza -> Int
Dada una pizza devuelve la cantidad de ingredientes
-}


cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

{-
armarPizza :: [Ingrediente] -> Pizza
Dada una lista de ingredientes construye una pizza
-}

armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

{-
sacarJamon :: Pizza -> Pizza
Le saca los ingredientes que sean jamón a la pizza
-}

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if (esJamon i) 
                        then sacarJamon p
                        else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

{-
tieneSoloSalsaYQueso :: Pizza -> Bool
Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
-}

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

{-
duplicarAceitunas :: Pizza -> Pizza
Recorre cada ingrediente y si es aceitunas duplica su cantidad
-}

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = if (esAceituna i) 
                                then Capa (duplicar i) (duplicarAceitunas p)
                                else Capa i (duplicarAceitunas p)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _             = False

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas n) = Aceitunas (n*2)
duplicar i             = i

{-
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente.
-}

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps


{-
2. Mapa de tesoros (con bifurcaciones)
Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
-}

data Dir = Izq | Der
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

{-
Definir las siguientes operaciones:
1. hayTesoro :: Mapa -> Bool
Indica si hay un tesoro en alguna parte del mapa.
-}

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroAca c 
hayTesoro (Bifurcacion c mi md) = hayTesoroAca c || hayTesoro mi || hayTesoro md

hayTesoroAca :: Cofre -> Bool
hayTesoroAca (Cofre os) = hayTesoroEn' os

hayTesoroEn' :: [Objeto] -> Bool
hayTesoroEn' []     = False
hayTesoroEn' (o:os) = esTesoro o || hayTesoroEn' os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

{-
2. hayTesoroEn :: [Dir] -> Mapa -> Bool
Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
lista vacía de direcciones.
-}

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c)             = hayTesoroAca c
hayTesoroEn [] (Bifurcacion c _ _) = hayTesoroAca c
hayTesoroEn (d:ds) (Fin _)         = False 
hayTesoroEn (d:ds) (Bifurcacion c mi md) = if (esIzq d) 
                                        then hayTesoroEn ds mi
                                        else hayTesoroEn ds md
                    
esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False


{-
3. caminoAlTesoro :: Mapa -> [Dir]
Indica el camino al tesoro. Precondición: existe un tesoro y es único.
-}

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c mi md) = if (hayTesoroAca c) 
                                        then []
                                        else if (hayTesoro mi)
                                            then Izq : caminoAlTesoro mi 
                                            else Der : caminoAlTesoro md


{-
4. caminoDeLaRamaMasLarga :: Mapa -> [Dir]
Indica el camino de la rama más larga.
-}

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = if (caminoMasLargo mi > caminoMasLargo md)
                                                then Izq : caminoDeLaRamaMasLarga mi
                                                else Der : caminoDeLaRamaMasLarga md
                                    
caminoMasLargo :: Mapa -> Int
caminoMasLargo (Fin c)               = 0
caminoMasLargo (Bifurcacion c mi md) = 1 + max (caminoMasLargo mi) (caminoMasLargo md)

{-
5. tesorosPorNivel :: Mapa -> [[Objeto]]
Devuelve los tesoros separados por nivel en el árbol.
-}

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [tesorosDe c]
tesorosPorNivel (Bifurcacion c mi md) = tesorosDe c : juntarTesorosPorNivel (tesorosPorNivel mi) (tesorosPorNivel md)

tesorosDe :: Cofre -> [Objeto]
tesorosDe (Cofre os) = tesoros os

tesoros :: [Objeto] -> [Objeto]
tesoros []     = []
tesoros (o:os) = if (esTesoro o) 
                then o : tesoros os
                else tesoros os

juntarTesorosPorNivel :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarTesorosPorNivel [] yss            = yss
juntarTesorosPorNivel xss []            = xss
juntarTesorosPorNivel (xs:xss) (ys:yss) = (xs++ys) : juntarTesorosPorNivel xss yss


{-
6. todosLosCaminos :: Mapa -> [[Dir]]
Devuelve todos lo caminos en el mapa.
-}

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c)               = [[]]
todosLosCaminos (Bifurcacion c mi md) = agregarCamino Izq (todosLosCaminos mi) ++ agregarCamino Der (todosLosCaminos md)

agregarCamino :: Dir -> [[Dir]] -> [[Dir]]
agregarCamino x []       = []
agregarCamino x (ys:yss) = (x:ys) : agregarCamino x yss

{-
3. Nave Espacial
modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:
-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
    
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show

{-
1. sectores :: Nave -> [SectorId]
Propósito: Devuelve todos los sectores de la nave.
-}

sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresEn ts

sectoresEn :: Tree Sector -> [SectorId]
sectoresEn EmptyT          = []
sectoresEn (NodeT s tsi tsd) = sectorIdDe s : (sectoresEn tsi) ++ (sectoresEn tsd)

sectorIdDe :: Sector -> SectorId
sectorIdDe (S sid _ _) = sid

{-
2. poderDePropulsion :: Nave -> Int
Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
el poder de propulsión es el número que acompaña al constructor de motores.
-}

poderDePropulsion :: Nave -> Int
poderDePropulsion (N ts) = poderDePropulsionDe ts

poderDePropulsionDe :: Tree Sector -> Int
poderDePropulsionDe EmptyT            = 0
poderDePropulsionDe (NodeT s tsi tsd) = poderDePropulsionActual s + poderDePropulsionDe tsi + poderDePropulsionDe tsd

poderDePropulsionActual :: Sector -> Int
poderDePropulsionActual (S _ cs _) = cantidadDePropulsion cs

cantidadDePropulsion :: [Componente] -> Int
cantidadDePropulsion []     = 0
cantidadDePropulsion (c:cs) = propulsion c + cantidadDePropulsion cs

propulsion :: Componente -> Int
propulsion (Motor p) = p
propulsion _         = 0

{-
3. barriles :: Nave -> [Barril]
Propósito: Devuelve todos los barriles de la nave.
-}

barriles :: Nave -> [Barril]
barriles (N ts) = barrilesEn ts

barrilesEn :: Tree Sector -> [Barril]
barrilesEn EmptyT            = []
barrilesEn (NodeT s tsi tsd) = barrilesDeSector s ++ barrilesEn tsi ++ barrilesEn tsd

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector (S _ cs _) = barrilesDe cs

barrilesDe :: [Componente] -> [Barril]
barrilesDe []     = []
barrilesDe (c:cs) = barril c ++ barrilesDe cs

barril :: Componente -> [Barril]
barril (Almacen bs) = bs
barril _            = []

{-
4. agregarASector :: [Componente] -> SectorId -> Nave -> Nave
Propósito: Añade una lista de componentes a un sector de la nave.
Nota: ese sector puede no existir, en cuyo caso no añade componentes.
-}

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N ts) = N (agregarComponentes cs sid ts)

agregarComponentes :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarComponentes cs sid EmptyT               = EmptyT
agregarComponentes cs sid (NodeT s tsi tsd) = if (esElMismoSector sid s) 
                                                then NodeT (agregarC cs s) tsi tsd 
                                                else NodeT s (agregarComponentes cs sid tsi) (agregarComponentes cs sid tsd)

esElMismoSector :: SectorId -> Sector -> Bool
esElMismoSector sid (S sid' _ _) = sid == sid'

agregarC :: [Componente] -> Sector -> Sector
agregarC cs (S sid cs' ts) = S sid (cs++cs') ts

{-
5. asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
Propósito: Incorpora un tripulante a una lista de sectores de la nave.
Precondición: Todos los id de la lista existen en la nave.
-}

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t sids (N ts) = N (asignarTripulanteASectores t sids ts)

asignarTripulanteASectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteASectores t [] ts         = ts
asignarTripulanteASectores t (sid:sids) ts = asignarTripulanteASectores t sids (agregarTripulanteA t sid ts)

agregarTripulanteA :: Tripulante -> SectorId -> Tree Sector -> Tree Sector
agregarTripulanteA t sid EmptyT            = EmptyT
agregarTripulanteA t sid (NodeT s tsi tsd) = if (sid == sectorIdDe s) 
                                                then NodeT (agregarTripulante t s) tsi tsd
                                                else NodeT s (agregarTripulanteA t sid tsi) (agregarTripulanteA t sid tsd)

agregarTripulante :: Tripulante -> Sector -> Sector
agregarTripulante t (S sid cs ts) = S sid cs (t:ts)

{-
6. sectoresAsignados :: Tripulante -> Nave -> [SectorId]
Propósito: Devuelve los sectores en donde aparece un tripulante dado.
-}

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresAsignadosDe t ts

sectoresAsignadosDe :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosDe t EmptyT            = []
sectoresAsignadosDe t (NodeT s tsi tsd) = if (existeTripulanteEn t s)
                                        then sectorIdDe s : sectoresAsignadosDe t tsi ++ sectoresAsignadosDe t tsd
                                        else sectoresAsignadosDe t tsi ++ sectoresAsignadosDe t tsd

existeTripulanteEn :: Tripulante -> Sector -> Bool
existeTripulanteEn t (S _ _ ts) = pertenece t ts

pertenece :: Tripulante -> [Tripulante] -> Bool
pertenece t []      = False
pertenece t (t':ts) = t == t' || pertenece t ts


{-
7. tripulantes :: Nave -> [Tripulante]
Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
-}

tripulantes :: Nave -> [Tripulante]
tripulantes (N ts) = sinRepetidos (tripulantesDe ts)

tripulantesDe :: Tree Sector -> [Tripulante]
tripulantesDe EmptyT            = []
tripulantesDe (NodeT s tsi tsd) =  tripulantesDeSector s ++ tripulantesDe tsi ++ tripulantesDe tsd

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S sid cs ts) = ts

sinRepetidos :: [Tripulante] -> [Tripulante]
sinRepetidos []     = []
sinRepetidos (t:ts) = if (pertenece t ts)
                        then sinRepetidos ts
                        else t : sinRepetidos ts

{-
4. Manada de lobos
Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:
Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
Las crías poseen sólo un nombre y no poseen lobos a cargo.
La estructura es la siguiente:
-}

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

{-
1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso
-}

manadaDeLobos :: Manada
manadaDeLobos = M lobos

lobos :: Lobo
lobos = Cazador "Braian" ["carne", "caballo", "alces"] expl1 expl2 cria1

expl1 :: Lobo
expl1 = Explorador "Matias" ["montaña fuji", "isla gerlapagos"] cria2 cria3

expl2 :: Lobo
expl2 = Explorador "Gaston" ["montaña 2", "montaña 5"] cria4 cria5

cria1 :: Lobo
cria1 = Cria "Sandro"

cria2 :: Lobo
cria2 = Cria "Pocho la pantera"

cria3 :: Lobo 
cria3 = Cria "Daniel Cardozo"

cria4 :: Lobo
cria4 = Cria "Chelo del grupo green"

cria5 :: Lobo
cria5 = Cria "Cristiano Ronaldo"


{-
2. buenaCaza :: Manada -> Bool
Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad
de crías.
-}

buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantidadDeAlimento l > cantidadDeCrias l

cantidadDeAlimento :: Lobo -> Int
cantidadDeAlimento (Cria n)                = 0
cantidadDeAlimento (Explorador n ts l1 l2) = cantidadDeAlimento l1 + cantidadDeAlimento l2
cantidadDeAlimento (Cazador n ps l1 l2 l3) = length ps + cantidadDeAlimento l1 + cantidadDeAlimento l2 + cantidadDeAlimento l3

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cria n)                = 1
cantidadDeCrias (Explorador n ts l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cazador n ps l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3

{-
3. elAlfa :: Manada -> (Nombre, Int)
Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
cero presas.
-}

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaEn l

elAlfaEn :: Lobo -> (Nombre, Int)
elAlfaEn (Cria n)                = (n, 0)
elAlfaEn (Explorador n ts l1 l2) = elMasAlfa (n, 0) (elMasAlfa (elAlfaEn l1) (elAlfaEn l2))
elAlfaEn (Cazador n ps l1 l2 l3) = elMasAlfa (elMasAlfa (n, length ps) (elAlfaEn l1)) (elMasAlfa (elAlfaEn l2) (elAlfaEn l3))

elMasAlfa :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elMasAlfa n1 n2 = 
    let (n, p)   = n1
        (n', p') = n2
    in if p > p'
       then n1
       else n2

{-
4. losQueExploraron :: Territorio -> Manada -> [Nombre]
Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
pasaron por dicho territorio.
-}

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronEn t l

losQueExploraronEn :: Territorio -> Lobo -> [Nombre]
losQueExploraronEn t (Cria n)                = []
losQueExploraronEn t (Explorador n ts l1 l2) = if (elem t ts) 
                                                then n : losQueExploraronEn t l1 ++ losQueExploraronEn t l2
                                                else losQueExploraronEn t l1 ++ losQueExploraronEn t l2
losQueExploraronEn t (Cazador n ps l1 l2 l3) = losQueExploraronEn t l1 ++ losQueExploraronEn t l2 ++ losQueExploraronEn t l3

{-
5. exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio
y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Los territorios no deben repetirse.
-}

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioEn l

exploradoresPorTerritorioEn :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioEn (Cria n)                 = []
exploradoresPorTerritorioEn (Explorador n ts l1 l2)  = juntarNombres (territoriosDe n ts)  (exploradoresPorTerritorioEn l1 ++ exploradoresPorTerritorioEn l2)
exploradoresPorTerritorioEn (Cazador n ps l1 l2 l3)  = exploradoresPorTerritorioEn l1 ++ exploradoresPorTerritorioEn l2 ++ exploradoresPorTerritorioEn l3

territoriosDe :: Nombre -> [Territorio] -> [(Territorio, Nombre)]
territoriosDe n []     = []
territoriosDe n (t:ts) = (t, n) : territoriosDe n ts

juntarNombres :: [(Territorio, Nombre)] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarNombres [] tns'         = tns'
juntarNombres (tn : tns) tns' = agruparNombres tn (juntarNombres tns tns')

agruparNombres :: (Territorio, Nombre) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agruparNombres tn []           =
    let (t, n) = tn in
        [(t,[n])]
agruparNombres tn (tns : tnss) = 
    let (t, n) = tn
        (t', ns) = tns in
            if t == t'
            then (t, n:ns) : tnss
            else (t', ns) : agruparNombres (t, n) tnss



{-
6. cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores
que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el
subordinado de un subordinado).
Precondición: hay un lobo con dicho nombre y es único.
Suponiendo la siguiente manada de ejemplo:
-}

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--PRECOND: hay un lobo con dicho nombre y es único.
cazadoresSuperioresDe n (M l) = if (estaEn n l)
                                then cazadoresSuperioresDeEn n l
                                else error "No existe el nombre dado"

cazadoresSuperioresDeEn :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresDeEn n' (Cria n)                = []
cazadoresSuperioresDeEn n' (Explorador n ts l1 l2) = cazadoresSuperioresDeEn n' l1 ++ cazadoresSuperioresDeEn n' l2
cazadoresSuperioresDeEn n' (Cazador n ps l1 l2 l3) = if (estaEn n' l1 || estaEn n' l2 || estaEn n' l3) 
                                                        then n : cazadoresSuperioresDeEn n' l1 ++ cazadoresSuperioresDeEn n' l2 ++ cazadoresSuperioresDeEn n' l3
                                                        else cazadoresSuperioresDeEn n' l1 ++ cazadoresSuperioresDeEn n' l2 ++ cazadoresSuperioresDeEn n' l3

estaEn :: Nombre -> Lobo -> Bool
estaEn n' (Cria n)                = n' == n
estaEn n' (Explorador n ts l1 l2) = n' == n || estaEn n' l1 || estaEn n' l2
estaEn n' (Cazador n ps l1 l2 l3) = n' == n || estaEn n' l1 || estaEn n' l2 || estaEn n' l3