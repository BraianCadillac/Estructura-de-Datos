{-
Introducción

Se desea modelar un sistema de documentos jerárquicos.

Un documento puede ser:

vacío
un bloque de texto
una sección con subtítulos (dos subsecciones)
-}

data Doc = Vacio
        | Texto Int Bool
        | Seccion String Doc Doc

{-
Interpretación
Texto n f
n = cantidad de palabras
f = si está formateado (True) o no (False)

Seccion titulo izq der
agrupa dos subdocumentos
-}


--EJERCICIO 1
cantidadBloques :: Doc -> Int
--Propósito: Cuenta la cantidad de nodos Texto.
cantidadBloques Vacio             = 0
cantidadBloques (Texto n f)       = 1
cantidadBloques (Seccion t di dd) = cantidadBloques di + cantidadBloques dd

--EJERCICIO 2
palabrasTotales :: Doc -> Int
--Propósito: Suma todas las palabras de los bloques de texto.
palabrasTotales Vacio             = 0
palabrasTotales (Texto n f)       = n
palabrasTotales (Seccion t di dd) = palabrasTotales di + palabrasTotales dd

--EJERCICIO 3
bloquesFormateados :: Doc -> Int
--Propósito: Cuenta cuántos bloques tienen True.
bloquesFormateados Vacio             = 0
bloquesFormateados (Texto n f)       = unoSi f
bloquesFormateados (Seccion t di dd) = bloquesFormateados di + bloquesFormateados dd

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

--EJERCICIO 4
titulos :: Doc -> [String]
--Propósito: Devuelve la lista de todos los títulos de las secciones.
titulos Vacio             = []
titulos (Texto n f)       = []
titulos (Seccion t di dd) = t : titulos di ++ titulos dd

--EJERCICIO 5
profundidadMax :: Doc -> Int
--Propósito: Devuelve la profundidad máxima del documento.
profundidadMax Vacio             = 0
profundidadMax (Texto n f)       = 0
profundidadMax (Seccion t di dd) = 1 + max (profundidadMax di) (profundidadMax dd)

--EJERCICIO 6
soloTextoPlano :: Doc -> Bool
--Propósito: Indica si todos los bloques de texto NO están formateados.
soloTextoPlano Vacio             = True
soloTextoPlano (Texto n f)       = not f
soloTextoPlano (Seccion t di dd) = soloTextoPlano di && soloTextoPlano dd

--EJERCICIO 7
duplicarSecciones :: Doc -> Doc
--Propósito: Duplica cada sección, es decir:
duplicarSecciones (Seccion t di dd) = Seccion t (Seccion t (duplicarSecciones di) Seccion t (duplicarSecciones dd))
duplicarSecciones d                 = d

--EJERCICIO 8
intercalarTexto :: Doc -> Doc
--Propósito: Inserta un bloque de texto vacío (Texto 0 False) entre cada combinación de secciones.
intercalarTexto Vacio             = Vacio
intercalarTexto (Texto n f)       = Texto 0 False
intercalarTexto (Seccion t di dd) = Seccion t (intercalarTexto di) (intercalarTexto dd)

--EJERCICIO 9
compactar :: Doc -> Doc
compactar Vacio = Vacio
compactar (Texto n f) = Texto n f
compactar (Seccion t di dd) = case (compactar di, compactar dd) of
                            (Texto n1 f1, Texto n2 f2) -> Texto (n1 + n2) (f1 || f2)
                            (di', dd') -> Seccion t di' dd'

espejo :: Doc -> Doc
espejo Vacio             = Vacio
espejo (Texto n f)       = Texto n (formateadoInvertido f)
espejo (Seccion t di dd) = Seccion t (espejo dd) (espejo di)

formateadoInvertido :: Bool -> Bool
formateadoInvertido True  = False
formateadoInvertido False = True 

