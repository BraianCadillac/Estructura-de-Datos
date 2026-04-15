{-
                                            1. Recursión sobre listas
 1. sumatoria :: [Int]-> Int
 Dada una lista de enteros devuelve la suma de todos sus elementos.
 2. longitud :: [a]-> Int
 Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
 de elementos que posee.
 3. sucesores :: [Int]-> [Int]
 Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
 4. conjuncion :: [Bool]-> Bool
 Dada una lista de booleanos devuelve True si todos sus elementos son True.
 5. disyuncion :: [Bool]-> Bool
 Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
 6. aplanar :: [[a]]-> [a]
 Dada una lista de listas, devuelve una única lista con todos sus elementos.
 7. pertenece :: Eq a => a-> [a]-> Bool
 Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
 a e.
 8. apariciones :: Eq a => a-> [a]-> Int
 Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
 9. losMenoresA :: Int-> [Int]-> [Int]
 Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
 10. lasDeLongitudMayorA :: Int-> [[a]]-> [[a]]
 Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
 de n elementos.
 11. agregarAlFinal :: [a]-> a-> [a]
 Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
 lista.
 12. agregar :: [a]-> [a]-> [a]
 Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
 elementos de la segunda a continuación. Definida en Haskell como (++).
 13. reversa :: [a]-> [a]
 Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
 en Haskell como reverse.
 14. zipMaximos :: [Int]-> [Int]-> [Int]
 Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
 máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
 las listas no necesariamente tienen la misma longitud.
 15. elMinimo :: Ord a => [a]-> a
 Dada una lista devuelve el mínimo
-}

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = succ n : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x then 1 + apariciones e xs
                        else apariciones e xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA m [] = []
losMenoresA m (n:ns) = if (n < m) 
                        then n : losMenoresA m ns
                        else losMenoresA m ns

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []       = []
lasDeLongitudMayorA n (xs:xss) = if (n < length xs) then xs : lasDeLongitudMayorA n xss
                                else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

agregar :: [a] -> [a] -> [a]
agregar []      ys = ys
agregar (x:xs) ys  = x : agregar xs ys

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []     ms      = ms
zipMaximos ns     []      = ns
zipMaximos (n:ns) (m:ms) = if (n >= m) 
                            then n : zipMaximos ns ms
                            else m : zipMaximos ns ms

elMinimo :: Ord a => [a] -> a
--PRECOND: En la lista dada hay un elemento y es el mínimo
elMinimo []     = error "No hay elementos en la lista"
elMinimo (x:xs) = if (null xs)
                then x
                else min x (elMinimo xs)

{-
                        2. Recursión sobre números
 Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
 lo contrario:
 1. factorial :: Int-> Int
 Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
 llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.

 2. cuentaRegresiva :: Int-> [Int]
 Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
 n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.

 3. repetir :: Int-> a-> [a]
 Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.

 4. losPrimeros :: Int-> [a]-> [a]
 Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
 Si la lista es vacía, devuelve una lista vacía.

 5. sinLosPrimeros :: Int-> [a]-> [a]
 Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
 recibida. Si n es cero, devuelve la lista completa.
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = if n >= 1 
                then n : cuentaRegresiva (n-1)
                else []

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

{-
                                         3. Registros
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:

mayoresA :: Int-> [Persona]-> [Persona]
Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
 
promedioEdad :: [Persona]-> Int
Dada una lista de personas devuelve el promedio de edad entre esas personas. 
Precondición: la lista al menos posee una persona.

elMasViejo :: [Persona]-> Persona
Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
lista al menos posee una persona.
-}

data Persona = P String Int
    deriving Show

yo = P "Braian" 32
tamara = P "Tamara" 28
lucas = P "Lucas" 39
maxi = P "Maxi" 40
graciela = P "Graciela" 68
jose = P "Jose" 69
tiziana = P "Tiziana" 8
nina = P "Nina" 4

personas = [yo, tamara, lucas, maxi, graciela, jose, tiziana, nina]

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (p:ps) = if (edad p > n) 
                    then p : mayoresA n ps
                    else mayoresA n ps
-------
promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumatoria (edades ps)) (length ps)

edades :: [Persona] -> [Int]
edades []     = []
edades (p:ps) = edad p : edades ps

edad :: Persona -> Int
edad (P n e) = e

-------

elMasViejo :: [Persona] -> Persona
--PRECOND: la lista dada posee al menos una persona y es la más vieja
elMasViejo []     = error "No hay personas en la lista"
elMasViejo (p:ps) = if (null ps)
                    then p
                    else case (edad p > edad (elMasViejo ps)) of
                                False -> elMasViejo ps
                                True  -> p

{-
 2. Modificaremos la representación de Entrenador y Pokemon de la práctica anterior de la siguiente manera:

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
Definir en base a esa representación las siguientes funciones:

cantPokemon :: Entrenador-> Int
Devuelve la cantidad de Pokémon que posee el entrenador.

cantPokemonDe :: TipoDePokemon-> Entrenador-> Int
Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.

cuantosDeTipo_De_LeGananATodosLosDe_:: TipoDePokemon-> Entrenador-> Entrenador-> Int
Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al
primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.

esMaestroPokemon :: Entrenador-> Bool
Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
-}

data TipoDePokemon  = Agua | Fuego | Planta
data Pokemon        = ConsPokemon TipoDePokemon Int
data Entrenador     = ConsEntrenador String [Pokemon]

charmander = ConsPokemon Fuego 90
squirtle = ConsPokemon Agua 56
bulbasaur = ConsPokemon Planta 87

ash   = ConsEntrenador "Ash" [charmander, charmander, bulbasaur]
misty = ConsEntrenador "Misty" [bulbasaur, bulbasaur]
brock = ConsEntrenador "Brock" [bulbasaur]

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n ps) = longitud ps

-------------

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp e = cantPokemonDeEn tp (pokemonsDe e)

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (ConsEntrenador n ps) = ps

cantPokemonDeEn :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeEn tp []     = 0
cantPokemonDeEn tp (p:ps) = unoSi (esTipoIgual tp (tipoPokemonDe p)) + cantPokemonDeEn tp ps

tipoPokemonDe :: Pokemon -> TipoDePokemon
tipoPokemonDe (ConsPokemon tp p) = tp

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

esTipoIgual :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoIgual Agua Agua     = True
esTipoIgual Fuego Fuego   = True
esTipoIgual Planta Planta = True
esTipoIgual _ _           = False


---------------
{-
Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al
primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.

data TipoDePokemon  = Agua | Fuego | Planta
data Pokemon        = ConsPokemon TipoDePokemon Int
data Entrenador     = ConsEntrenador String [Pokemon]
-}

cuantosDeTipo_De_LeGananATodosLosDe_:: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tp (ConsEntrenador n1 ps1) (ConsEntrenador n2 ps2) = cuantos_De_LeGananA_ tp ps1 ps2


cuantos_De_LeGananA_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantos_De_LeGananA_ tp [] pks     = 0
cuantos_De_LeGananA_ tp (p:ps) pks = unoSi ((esTipoIgual tp (tipoDe p)) && superaATodos p pks) + cuantos_De_LeGananA_ tp ps pks

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos pk []     = True
superaATodos pk (p:ps) = superaA pk p && superaATodos pk ps


superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esSuperiorA (tipoDe p1) (tipoDe p2)

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t e) = t

esSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperiorA Agua Fuego      = True
esSuperiorA Fuego Planta    = True
esSuperiorA Planta Agua     = True
esSuperiorA _ _             = False

---------------


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = hayDeEn Agua (pokemonsDe e) && hayDeEn Planta (pokemonsDe e) && hayDeEn Fuego (pokemonsDe e)

hayDeEn :: TipoDePokemon -> [Pokemon] -> Bool
hayDeEn tp []     = False
hayDeEn tp (p:ps) = esTipoIgual tp (tipoPokemonDe p) || hayDeEn tp ps



{-
3. El tipo de dato Rol representa los roles (desarrollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La de nición es la siguiente:

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

Definir las siguientes funciones sobre el tipo Empresa:

proyectos :: Empresa-> [Proyecto]
Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.

losDevSenior :: Empresa-> [Proyecto]-> Int
Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertenecen
además a los proyectos dados por parámetro.

cantQueTrabajanEn :: [Proyecto]-> Empresa-> Int
Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.

asignadosPorProyecto :: Empresa-> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.
-}

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto  = ConsProyecto String
    deriving Show
data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa   = ConsEmpresa [Rol]
    deriving Show


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa e) = sinRepetidos (proyectosDe e)

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe []     = []
proyectosDe (r:rs) = proyecto r : proyectosDe rs

proyecto :: Rol -> Proyecto
proyecto (Developer _ p)  = p
proyecto (Management _ p) = p

sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos []     = []
sinRepetidos (p:ps) = if not (seEncuentraEn p ps) 
                        then p : sinRepetidos ps
                        else sinRepetidos ps

seEncuentraEn :: Proyecto -> [Proyecto] -> Bool
seEncuentraEn p []       = False
seEncuentraEn p (pr:prs) = nombreProyecto p == nombreProyecto pr || seEncuentraEn p prs

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n

-------------------------------
{-
losDevSenior :: Empresa-> [Proyecto]-> Int
Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertenecen
además a los proyectos dados por parámetro.

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto  = ConsProyecto String
    deriving Show
data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa   = ConsEmpresa [Rol]
    deriving Show
-}
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa r) ps = cantidadDeDevSenior r ps

cantidadDeDevSenior :: [Rol] -> [Proyecto] -> Int
cantidadDeDevSenior []     ps = 0
cantidadDeDevSenior (r:rs) ps = unoSi (esDevSenior r && participaEnAlgunProyecto r ps) + cantidadDeDevSenior rs ps

participaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
participaEnAlgunProyecto r []     = False
participaEnAlgunProyecto r (p:ps) = participa r p || participaEnAlgunProyecto r ps

participa :: Rol -> Proyecto -> Bool
participa (Developer s p) pr = nombreProyecto p == nombreProyecto pr
participa _ pr               = False

esDevSenior :: Rol -> Bool
esDevSenior (Developer s p) = esSenior s
esDevSenior _               = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False

-----------------------------------

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = losQueTrabajanEn rs ps

losQueTrabajanEn :: [Rol] -> [Proyecto] -> Int
losQueTrabajanEn [] ps     = 0
losQueTrabajanEn (r:rs) ps = unoSi (seEncuentraEn (proyecto r) ps) + losQueTrabajanEn rs ps

-----------------------------------

{-
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.
-}

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa r) = proyectosDeLaEmpresa r

proyectosDeLaEmpresa :: [Rol] -> [(Proyecto, Int)]
proyectosDeLaEmpresa []     =  []
proyectosDeLaEmpresa (r:rs) = juntarProyecto (proyecto r, 1) (proyectosDeLaEmpresa rs)

juntarProyecto :: (Proyecto, Int) -> [(Proyecto, Int)] -> [(Proyecto, Int)]
juntarProyecto (p, c) []         = [(p,c)]
juntarProyecto (p, c) (pc : pcs) = 
                        let (p', c') = pc in 
                            if nombreProyecto p == nombreProyecto p' 
                            then (p', c'+c) : pcs
                            else pc : juntarProyecto (p,c) pcs