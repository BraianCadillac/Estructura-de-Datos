{-
2. Números enteros
1. Defina las siguientes funciones:

a) sucesor :: Int -> Int
Dado un número devuelve su sucesor
-}

sucesor :: Int -> Int
sucesor x = x+1

{-
b) sumar :: Int -> Int -> Int
Dados dos números devuelve su suma utilizando la operación +.
-}

sumar :: Int -> Int -> Int
sumar x y = x+y

{-
c) divisionYResto :: Int -> Int -> (Int, Int)
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.
-}

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

{-
d) maxDelPar :: (Int,Int) -> Int
Dado un par de números devuelve el mayor de estos.
-}

maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) = max x y

{-
2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDelPar (divisionYResto (suma 5 5) (sucesor 0))
-}

{-
maxDelPar (divisionYResto (sumar 9 1) (sucesor 0))
sumar (maxDelPar (divisionYResto 18 2)) (sucesor 0)
maxDelPar (divisionYResto (suma 5 5) (sucesor 0))
sucesor (sumar (maxDelPar (divisionYResto 16 2)) 1)
-}


{-
3. Tipos Enumerativos

c) siguiente :: Dir -> Dir
Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
total o parcial? ¾Por qué?
-}

data Dir = Norte | Sur | Este | Oeste
    deriving Show

{-
a) opuesto :: Dir -> Dir
Dada una dirección devuelve su opuesta.
-}

opuesto :: Dir -> Dir
opuesto d = case d of
            Norte -> Sur
            Sur   -> Norte
            Este  -> Oeste
            Oeste -> Este


{-
b) iguales :: Dir -> Dir -> Bool
Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
-}

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

{-
c) siguiente :: Dir -> Dir
Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
total o parcial? ¿Por qué?
Es una función parcial, porque la siguiente dirección a Oeste no existe, ¡falla!
-}

siguiente :: Dir -> Dir
--PRECOND: La siguiente dirección a Oeste no existe
siguiente d = case d of 
            Norte -> Este
            Este  -> Sur
            Sur   -> Oeste
            Oeste -> error "La siguiente direccion a Oeste no existe" 


{-
2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
-}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

{-
a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.
-}

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

{-
b) empiezaConM :: DiaDeSemana -> Bool
Dado un día de la semana indica si comienza con la letra M.
-}

empiezaConM :: DiaDeSemana -> Bool
empiezaConM dds = case dds of
                Lunes     -> False
                Martes    -> True
                Miercoles -> True
                Jueves    -> False
                Viernes   -> False
                Sabado    -> False
                Domingo   -> False


{-
c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
Ejemplo: vieneDespues Jueves Lunes = True
-}

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dds1 dds2 = numeroDia dds1 > numeroDia dds2

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia Domingo   = 7

{-
d) estaEnElMedio :: DiaDeSemana -> Bool
Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
-}

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

{-
3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
-}

{-
a) negar :: Bool -> Bool
Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.
-}

negar :: Bool -> Bool
negar True  = False
negar False = True

{-
b) implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.
-}

implica :: Bool -> Bool -> Bool
implica False b2 = True
implica b1 True  = True
implica True _   = False

{-
c) yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.
-}

yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien _ False = False
yTambien _ _     = True

{-
d) oBien :: Bool -> Bool -> Bool
Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.
-}

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _    = False

{-
4. Registros
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
-}

yo = P "Braian" 33
ella = P "Tamara" 33

data Persona = P String Int 
            --Nombre Edad
    deriving Show

{-
nombre :: Persona -> String
Devuelve el nombre de una persona
-}

nombre :: Persona -> String
nombre (P n e) = n

{-
edad :: Persona -> Int
Devuelve la edad de una persona
-}

edad :: Persona -> Int
edad (P n e) = e

{-
crecer :: Persona -> Persona
Aumenta en uno la edad de la persona.
-}

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

{-
cambioDeNombre :: String -> Persona -> Persona
Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
nuevo nombre.
-}

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P n e) = P nn e

{-
esMayorQueLaOtra :: Persona -> Persona -> Bool
Dadas dos personas indica si la primera es mayor que la segunda.
-}

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

{-
laQueEsMayor :: Persona -> Persona -> Persona
Dadas dos personas devuelve a la persona que sea mayor.
-}

laQueEsMayor :: Persona -> Persona -> Persona
--OBSERVACIONES: Si edad de p1 y p2 son iguales, p2 es el mayor
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
                     then p1
                     else p2

{-
2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:
-}

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = Pok TipoDePokemon Int
    deriving Show

data Entrenador = E String Pokemon Pokemon
    deriving Show

{-
superaA :: Pokemon -> Pokemon -> Bool
Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
-}

charmander = Pok Fuego 90
squirtle   = Pok Agua 70
bulbasaur  = Pok Planta 80 

ash = E "ash" charmander squirtle
brock = E "brock" squirtle bulbasaur

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esSuperior (tipoDePokemonDe p1) (tipoDePokemonDe p2)

tipoDePokemonDe :: Pokemon -> TipoDePokemon
tipoDePokemonDe (Pok tp p) = tp 

esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua Fuego   = True
esSuperior Fuego Planta = True
esSuperior Planta Agua  = True
esSuperior _ _          = False

{-
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
-}

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E n p1 p2) = unoSi tp (tipoDePokemonDe p1) + unoSi tp (tipoDePokemonDe p2)

unoSi :: TipoDePokemon -> TipoDePokemon -> Int
unoSi Agua Agua     = 1
unoSi Planta Planta = 1
unoSi Fuego Fuego   = 1
unoSi _ _           = 0

{-
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
-}

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = (juntarPokemonDe e1) ++ (juntarPokemonDe e2)

juntarPokemonDe :: Entrenador -> [Pokemon]
juntarPokemonDe (E n p1 p2) = p1:p2:[]

{-
5. Funciones polimórficas
1. Defina las siguientes funciones polimórficas:

a) loMismo :: a -> a
Dado un elemento de algún tipo devuelve ese mismo elemento.
-}

loMismo :: a -> a
loMismo a = a

{-
b) siempreSiete :: a -> Int
Dado un elemento de algún tipo devuelve el número 7.
-}

siempreSiete :: a -> Int
siempreSiete a = 7

{-
c) swap :: (a,b) -> (b, a)
Dadas una tupla, invierte sus componentes.
¿Por qué existen dos variables de tipo diferentes?
Porque es poliformismo paramétrico, permite definir estructuras de datos genéricas 

2. Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?
Porque contiene variables que justamente son "contenedores" de cualquier tipo de dato, es decir por ejemplo la variable a puede ser un Int, un String, etc. No es necesario redefinir funciones, con usar funciones polimórficas nos permite ahorrar código
--
-}

swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

{-
6. Pattern matching sobre listas
1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):
-}

{-
2. estaVacia :: [a] -> Bool
Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
Definida en Haskell como null.
-}

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

{-
3. elPrimero :: [a] -> a
Dada una lista devuelve su primer elemento.
Definida en Haskell como head.
Nota: tener en cuenta que el constructor de listas es :
-}

elPrimero :: [a] -> a
--PRECOND: Hay almenos un elemento en la lista dada
elPrimero (x:_) = x

{-
4. sinElPrimero :: [a] -> [a]
Dada una lista devuelve esa lista menos el primer elemento.
Definida en Haskell como tail.
Nota: tener en cuenta que el constructor de listas es :
-}

sinElPrimero :: [a] -> [a]
--PRECOND: Hay al menos un elemento en la lista dada
sinElPrimero (x:xs) = xs

{-
5. splitHead :: [a] -> (a, [a])
Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
lista, y la segunda componente es esa lista pero sin el primero.
Nota: tener en cuenta que el constructor de listas es :
-}

splitHead :: [a] -> (a, [a])
--PRECOND: Hay almenos un elemento en la lista dada
splitHead (x:xs) = (x, xs)