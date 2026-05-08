
type Software = String
data Autor = Admin String | Dev String
data Organizador = Agregar Software [Autor] Organizador | Vacio

--Ejercicios
--Resolver las siguientes funciones utilizando recursión estructural sobre la estructura del tipo Organizador:

pares :: Organizador -> [(Software, Int)] -- costo de la operación O(A^2)
--length = O(A) donde A es la cantidad de autores de la lista
--Propósito: dado un organizador, denota el conjunto de pares programa y cantidad de autores que existen.
pares Vacio            = []
pares (Agregar s as o) = (s, length as) : pares o

enComun :: Autor -> Autor -> Organizador -> [Software] -- costo de la operación O(A^2)
--perteneceAlMismoS = O(A) 
--Propósito: dados dos autores y un organizador, denota el conjunto de aquellos programas en los que ambos participaron.
enComun a1 a2 Vacio            = []
enComun a1 a2 (Agregar s as o) = if (pertenecenAlMismoS a1 a2 as)
                                then s : enComun a1 a2 o
                                else enComun a1 a2 o

pertenecenAlMismoS :: Autor -> Autor -> [Autor] -> Bool -- O(A) + O(A) = O(A)
pertenecenAlMismoS a1 a2 as = elem a1 as && elem a2 as

filtrar :: [Autor] -> Organizador -> Organizador --por cada software hago borrarAutores sino me equivoco sería = O(S*(N^2))
--Propósito: dado un conjunto de autores y un organizador, elimina esos autores de cada software.
filtrar as Vacio             = Vacio
filtrar as (Agregar s as' o) = Agregar s (borrarAutores as as') (filtrar as o)

borrarAutores :: [Autor] -> [Autor] -> [Autor] -- O(A^2) costo cuadrático, por cada autor a borrar hago una operación lineal
--borrarAutor = O(A)
borrarAutores [] as'     = as'
borrarAutores (a:as) as' = borrarAutor a as' ++ (borrarAutores as as')

borrarAutor :: Autor -> [Autor] -> [Autor] --O(A) donde A es la cantidad de autores de la lista en peor caso, cada autor A hace una dos operaciones constantes
borrarAutor a []       = []
borrarAutor a (a':as') = if (a==a')
                        then as'
                        else a' : (borrarAutor a as')

losAdmin :: Organizador -> [Autor] -- por cada software hacemos una operación cuadrática : O(A + A^2) = O(A^2)
--sinRepetidos = O(A)
--Propósito: denota una lista con todos los administradores, sin elementos repetidos.
losAdmin Vacio             = []
losAdmin (Agregar s as o) = sinRepetidos (administradores as ++ (losAdmin o))

administradores :: [Autor] -> [Autor] --O(A) donde A es la cantidad de autores de la lista. Por cada autor A hacemos operaciones constantes
administradores []     = []
administradores (a:as) = if (esAdminitrador a)
                        then a : administradores as
                        else administradores as

esAdminitrador :: Autor -> Bool --O(1)
esAdminitrador (Admin _) = True
esAdminitrador _         = False

sinRepetidos :: [Autor] -> [Autor] --O(A) donde A es la cantidad de autores de la lista, por cada A hacemos una operación constante
sinRepetidos []     = []
sinRepetidos (a:as) = if (elem a as)
                        then sinRepetidos as
                        else a : sinRepetidos as

ordenados :: Organizador -> [Software]
--Propósito: dado un organizador, denota la lista de programas ordenados de menor a mayor por cantidad de autores.
ordenados o = softwaresO (ordenarPares (pares o))

ordenarPares :: [(Software, Int)] -> [(Software, Int)]
ordenarPares []       = []
ordenarPares (si:sis) = ordenarPar si (ordenarPares sis)

ordenarPar :: (Software, Int) -> [(Software, Int)] -> [(Software, Int)]
ordenarPar si []          = [si]
ordenarPar si (si': sis') = let (s, i) = si
                            (s',i') = si' in
                                if i < i'
                                then si : si' : sis'
                                else si' : ordenarPar si sis'

softwaresO :: [(Software, Int)] -> [Software]
softwaresO []       = []
softwaresO (si:sis) = let (s,i) = si in
    s : softwaresO sis