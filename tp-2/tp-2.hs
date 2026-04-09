{-
1. Recursión sobre listas
Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
lo contrario:

1. sumatoria :: [Int] -> Int
Dada una lista de enteros devuelve la suma de todos sus elementos.
-}

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

{-
2. longitud :: [a] -> Int
Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee.
-}

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

{-
3. sucesores :: [Int] -> [Int]
Dada una lista de enteros, devuelve la lista de los sucesores de cada entero
-}

sucesor :: Int -> Int
sucesor x = x+1
{-REUTILIZO FUNCION DEL TP-1-}

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = sucesor n : sucesores ns

{-
4. conjuncion :: [Bool] -> Bool
Dada una lista de booleanos devuelve True si todos sus elementos son True.
-}

conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

{-
5. disyuncion :: [Bool] -> Bool
Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
-}

disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

{-
6. aplanar :: [[a]] -> [a]
Dada una lista de listas, devuelve una única lista con todos sus elementos.
-}

aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (xs:xss) = xs ++ aplanar xss

{-
7. pertenece :: Eq a => a -> [a] -> Bool
Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
a e.
-}

pertenece :: Eq a => a -> [a] -> Bool
pertenece a []     = False
pertenece a (x:xs) = a==x || pertenece a xs

{-
8. apariciones :: Eq a => a -> [a] -> Int
Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
-}

apariciones :: Eq a => a -> [a] -> Int
apariciones a []     = 0
apariciones a (x:xs) = if (a==x) 
                        then 1 + apariciones a xs
                        else apariciones a xs


{-
9. losMenoresA :: Int -> [Int] -> [Int]
Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
-}

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = if (x < n)
                        then x : losMenoresA n xs
                        else losMenoresA n xs


{-
10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos.
-}

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []       = []
lasDeLongitudMayorA n (ns:nss) = if (longitud ns > n)
                                then ns : lasDeLongitudMayorA n nss
                                else lasDeLongitudMayorA n nss


{-
11. agregarAlFinal :: [a] -> a -> [a]
Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista.
-}

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs y = xs ++ [y]

{-
12. agregar :: [a] -> [a] -> [a]
Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).
-}

agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys
agregar (x:xs) ys = x : (agregar xs ys)

{-
13. reversa :: [a] -> [a]
Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
en Haskell como reverse.
-}

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = reversa xs ++ [x]

{-
14. zipMaximos :: [Int] -> [Int] -> [Int]
Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
-}

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys         = []
zipMaximos xs []         = []
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys

{-
15. elMinimo :: Ord a => [a] -> a
Dada una lista devuelve el mínimo
-}

elMinimo :: Ord a => [a] -> a
--PRECOND: Hay al menos un elemento en la lista y es el minimo
elMinimo [x]    = x
elMinimo (x:xs) = min x (elMinimo xs)

{-
2. Recursión sobre números
Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
lo contrario:

1. factorial :: Int -> Int
Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

{-
2. cuentaRegresiva :: Int -> [Int]
Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
-}

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

{-
3. repetir :: Int -> a -> [a]
Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
-}

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

{-
4. losPrimeros :: Int -> [a] -> [a]
Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía.
-}

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

{-
5. sinLosPrimeros :: Int -> [a] -> [a]
Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa.
-}

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


{-
3. Registros
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
-}

data Persona = P String Int 
            --Nombre Edad
    deriving Show

{-
mayoresA :: Int -> [Persona] -> [Persona]
Dados una edad y una lista de personas devuelve a las personas mayores a esa edad
-}

yo    = P "Braian" 33
ella  = P "Tamara" 30
maxi  = P "Maxi" 40
lucas = P "Lucas" 38

familia = [yo, ella, maxi, lucas]

edad :: Persona -> Int
edad (P n e) = e



mayoresA :: Int -> [Persona] -> [Persona]
mayoresA e []     = []
mayoresA e (p:ps) = if (edad p > e)
                    then p : mayoresA e ps
                    else mayoresA e ps


{-
promedioEdad :: [Persona] -> Int
Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
-}

promedioEdad :: [Persona] -> Int
--PRECOND: La lista dada posee al menos una persona
promedioEdad ps = div (sumatoria (edadesDe ps)) (longitud ps)

edadesDe :: [Persona] -> [Int]
edadesDe []     = []
edadesDe (p:ps) = edad p : edadesDe ps

{-
elMasViejo :: [Persona] -> Persona
Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
lista al menos posee una persona.
-}

elMasViejo :: [Persona] -> Persona
--PRECOND: La lista dada posee al menos una persona y es el mas viejo
elMasViejo [p]    = p
elMasViejo (p:ps) = if (edad p >  edad (elMasViejo ps))
                    then p
                    else elMasViejo ps


{-
2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:
-}

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
data Pokemon       = ConsPokemon TipoDePokemon Int
    deriving Show
data Entrenador    = ConsEntrenador String [Pokemon]
    deriving Show


charmander = ConsPokemon Fuego 90
squirtle   = ConsPokemon Agua 70
bulbasaur  = ConsPokemon Planta 80 

ash   = ConsEntrenador "ash" [charmander, squirtle, bulbasaur, charmander]
brock = ConsEntrenador "brock" [squirtle, bulbasaur]

{-
cantPokemon :: Entrenador -> Int
Devuelve la cantidad de Pokémon que posee el entrenador.
-}

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n ps) = longitud ps

{-
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
-}

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (ConsEntrenador n ps) = cantidadPokemonDeTipo tp ps

cantidadPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantidadPokemonDeTipo tp []     = 0
cantidadPokemonDeTipo tp (p:ps) = unoSiEs tp (tipoDe p) + cantidadPokemonDeTipo tp ps

unoSiEs :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEs Agua Agua     = 1
unoSiEs Fuego Fuego   = 1
unoSiEs Planta Planta = 1
unoSiEs _ _           = 0

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon tp p) = tp

{-
cuantosDeTipo_De_LeGananATodosLosDe_
:: TipoDePokemon -> Entrenador -> Entrenador -> Int
Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al
primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.
-}

{-REUTILIZO FUNCIONES DEL TP-1-}
superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esSuperior (tipoDe p1) (tipoDe p2)

esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua Fuego   = True
esSuperior Fuego Planta = True
esSuperior Planta Agua  = True
esSuperior _ _          = False


cuantosDeTipo_De_LeGananATodosLosDe_:: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tp e1 e2 = cuantosDe_LesGananATodosDe tp (pokemonsDe e1) (pokemonsDe e2)

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (ConsEntrenador n ps) = ps

cuantosDe_LesGananATodosDe:: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosDe_LesGananATodosDe tp []     ps' = 0
cuantosDe_LesGananATodosDe tp (p:ps) ps' = unoSiGanaATodos (esTipo tp (tipoDe p) && leGanaATodos p ps') + cuantosDe_LesGananATodosDe tp ps ps'

esTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esTipo Agua Agua     = True
esTipo Fuego Fuego   = True
esTipo Planta Planta = True
esTipo _ _           = False

unoSiGanaATodos :: Bool -> Int
unoSiGanaATodos True  = 1
unoSiGanaATodos False = 0

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p []       = True
leGanaATodos p (p':ps') = superaA p p' && leGanaATodos p ps'


{-
esMaestroPokemon :: Entrenador -> Bool
Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
-}

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador n ps) = hayUnoDe_En_ Agua ps && hayUnoDe_En_ Fuego ps && hayUnoDe_En_ Planta ps

hayUnoDe_En_:: TipoDePokemon -> [Pokemon] -> Bool
hayUnoDe_En_ tp []     = False
hayUnoDe_En_ tp (p:ps) = esTipo tp (tipoDe p) || hayUnoDe_En_ tp ps


{-
3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La definición es la siguiente:
-}

data Seniority = Junior | SemiSenior | Senior
data Proyecto  = ConsProyecto String
data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa   = ConsEmpresa [Rol]

{-
proyectos :: Empresa -> [Proyecto]
Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
-}

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinRepetidos (proyectosDe rs)

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe []     = []
proyectosDe (r:rs) = proyectoDe r : proyectosDe rs

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p)  = p
proyectoDe (Management _ p) = p





sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos []     = []
sinRepetidos (p:ps) = 
                    let nombres = nombresProyectos ps in
                    if (elem (nombreProyecto p) nombres)
                        then sinRepetidos ps
                        else p : sinRepetidos ps


nombresProyectos :: [Proyecto] -> [String]
nombresProyectos []     = []
nombresProyectos (p:ps) = nombreProyecto p : nombresProyectos ps

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n


{-
losDevSenior :: Empresa -> [Proyecto] -> Int
Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.
-}

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = devSeniors rs ps

devSeniors :: [Rol] -> [Proyecto] -> Int
devSeniors [] ps     = 0
devSeniors (r:rs) ps = unoSiEsDevYPertenece (esDevSenior r && seEncuentraEn (proyectoDe r) ps) + devSeniors rs ps



esDevSenior :: Rol -> Bool
esDevSenior (Developer s _) = esSenior s
esDevSenior _               = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False

seEncuentraEn :: Proyecto -> [Proyecto] -> Bool
seEncuentraEn p []      = False
seEncuentraEn p (p':ps) = nombreProyecto p == nombreProyecto p' || seEncuentraEn p ps


unoSiEsDevYPertenece :: Bool -> Int
unoSiEsDevYPertenece True  = 1
unoSiEsDevYPertenece False = 0

{-
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.
-}

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = asignadosPorProyectoDe (sinRepetidos(proyectos e)) (empleados e)

empleados :: Empresa -> [Rol]
empleados (ConsEmpresa rs) = rs

asignadosPorProyectoDe :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
asignadosPorProyectoDe [] rs     = []
asignadosPorProyectoDe (p:ps) rs = (p, cantidadDeAsignadosEn p rs) : asignadosPorProyectoDe ps rs

cantidadDeAsignadosEn :: Proyecto -> [Rol] -> Int
cantidadDeAsignadosEn p []     = 0
cantidadDeAsignadosEn p (r:rs) = unoSi (nombreProyecto p == nombreProyecto (proyectoDe r)) + cantidadDeAsignadosEn p rs


unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

