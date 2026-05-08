data Contenedor = Comida | Oxigeno | Torpedo | Combustible

data Componente = Escudo | CanonLaser | Lanzatorpedos | Motor Int | Almacen [Contenedor]

data Nave = Parte Componente Nave Nave | ParteBase

--Ejercicio 1 Construya un valor de tipo Nave que contenga un motor, un escudo, dos armas y un almacen que posea comida.

nave :: Nave
nave = Parte (Motor 50) (Parte Escudo (Parte (Almacen [Comida]) ParteBase ParteBase) ParteBase) (Parte CanonLaser (Parte Lanzatorpedos ParteBase ParteBase) ParteBase)

--Resuelva las siguientes funciones, con recursión estructural sobre el tipo Nave:

componentes :: Nave -> [Componente]
--PROPÓSITO: Retorna la lista de componentes.
componentes ParteBase       = []
componentes (Parte c ni nd) = c : (componentes ni) ++ (componentes nd)

poderDePropulsion :: Nave -> Int
--Retorna el poder de propulsión de una nave. El poder de propulsión de una nave es la suma de los poderes de propulsión de los motores de la nave.
poderDePropulsion ParteBase       = 0
poderDePropulsion (Parte c ni nd) = poder c + (poderDePropulsion ni) + (poderDePropulsion nd)

poder :: Componente -> Int
poder (Motor p) = p
poder _         = 0

desarmarse :: Nave -> Nave
--PROPÓSITO: Reemplaza armas por escudos.
desarmarse ParteBase       = ParteBase
desarmarse (Parte c ni nd) = Parte (desarmar c) (desarmarse ni) (desarmarse nd)

desarmar :: Componente -> Componente
desarmar CanonLaser    = Escudo
desarmar Lanzatorpedos = Escudo
desarmar c             = c


cantidadComida :: Nave -> Int --O(C*(CO)) donde C es la cantidad total de Componentes en la nave. En peor caso por cada componente C, hace una operación CO que es la cantidad de comida del contenedor
--cantidadDeComidaAca = O(CO)
--PROPÓSITO: Dada una nave devuelve la cantidad de comida. Cada aparición de Comida vale 1.
cantidadComida ParteBase       = 0
cantidadComida (Parte c ni nd) = cantidadDeComidaAca c + cantidadComida ni + cantidadComida nd

cantidadDeComidaAca :: Componente -> Int --O(CO)
--cantidad = O(CO)
cantidadDeComidaAca (Almacen c) = cantidad c
cantidadDeComidaAca _           = 0

cantidad :: [Contenedor] -> Int --costo de la operación = O(CO) donde CO es la cantidad de contenedores de la lista, por cada contenedor CO se hacen dos operaciones constantes
--unoSi = O(1)
--esComida = O(1)
cantidad []     = 0
cantidad (c:cs) = unoSi (esComida c) + cantidad cs

esComida :: Contenedor -> Bool --O(1)
esComida Comida = True
esComida _      = False

unoSi :: Bool -> Int --O(1)
unoSi True  = 1
unoSi False = 0

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

naveToTree :: Nave -> Tree Componente
--PROPÓSITO: Dada una nave la transforma en un árbol de componentes
naveToTree ParteBase       = EmptyT
naveToTree (Parte c ni nd) = NodeT c (naveToTree ni) (naveToTree nd)

aprovisionados :: [Contenedor] -> Nave -> Bool
--PROPÓSITO: dada una lista de contenedores chequea que cada almacén
aprovisionados cs ParteBase = True
aprovisionados cs (Parte comp ni nd) = tieneTodo cs comp && aprovisionados cs ni && aprovisionados cs nd

tieneTodo :: Componente -> Bool
tieneTodo (Almacen cs') = tieneTodoEn cs cs'
tieneTodo _             = True

tieneTodoEn :: [Contenedor] -> [Contenedor] -> Bool
tieneTodoEn [] cs'     = True
tieneTodoEn (c:cs) cs' = elem c cs' && tieneTodoEn cs cs'

armasNivelN :: Int -> Nave -> [Componente]
--PROPÓSITO: Devuelve las armas que haya en el nivel "n" de la nave.
armasNivelN 0 ParteBase       = []
armasNivelN 0 (Parte c ni nd) = singularSi c
armasNivelN n (Parte c ni nd) = (armasNivelN (n-1) ni) ++ (armasNivelN (n-1) nd) 

singularSi :: Componente -> [Componente]
singularSi CanonLaser    = [CanonLaser]
singularSi Lanzatorpedos = [Lanzatorpedos]
singularSi _             = []