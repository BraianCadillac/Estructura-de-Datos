Data MVTree = MVT (Heap (Int, Producto)) (Map categoria MVTree)
                    --vendidos            subcategorias
    {-
    INV REPR: En (MVT hip mcm) se cumple que:
            *En cada hip, en (i, p) no hay dos productos con el mismo nombre
            *En cada hip, en (i, p) donde i es la cantidad de productos, se cumple que i>0
            *En mcm, Si un producto aparece en alguna categoría descendiente,
                debe aparecer en la heap actual con la suma total correspondiente
    -}

{-
Donde cada nodo del árbol contiene una heap de todos los productos de esa categoría y sus subcategorías, con la cantidad vendida para ese producto y un map con los subárboles correspondientes a las subcategorías. Cuando una categoría no tiene subcategorías, el map está vacío. Una condición importante es que cada una de las heaps que aparecen en el árbol debe tener solamente un elemento (n, p) para cada producto p (o sea, no hay en la misma heap dos elementos (n,p) y (m,p) para el mismo p) cabe aclarar que el árbol principal representa a una categoría madre sin nombre, de las que todas las categorías principales son hijas.
Observación: Esta representación persigue solo fines didácticos.
-}



{-
Type Categoria = String
Type CaminoJ = [Categoria]
Type Producto = String
-}

emptyMVTree :: MVtree --O(1)
--PROP: Representa a una jerarquía de categorías sin categorías
--Eficiencia: calcular y justificar
emptyMVTree = MVT emptyH emptyM

{-
que dado un camino jerárquico y una jerarquía de categoría, describe la jerarquía dada de forma tal que todas las mencionadas en el camino existan como subcategorías de la anterior (si ya existía, pueden agregar las subcategorías necesarias, y si no, se crean sin producto
-}

registrarCategoria :: CaminoJ -> MVTree -> MVTree --O(J*(log SC)) por cada J del camino jerarquico se hacen dos operaciones logaritmicas
--lookupM = O(log SC) siendo SC la cantidad de sub categorias del map
--assocM = O(log SC) donde SC la cantidad de sub categorias del map
registrarCategoria [] t                = t
registrarCategoria (c:cs) (MVT hip mcm) = case lookupM c mcm of
                                        Nothing -> let hijo = registrarCategoria cs emptyMVTree in 
                                            MVT hip (assocM c hijo mcm)
                                        Just sub -> let sub' = registrarCategoria cs sub in 
                                            MVT hip (assocM c sub' mcm)

--SC a la máxima cantidad de subcategorías
--P a la máxima cantidad de productos
--J a la longitud de un camino jerárquico

{-
que dado un comino jerárquico y una jerarquía de categorías, describe la lista de todas las subcategorías de la última categoría del camino. Se supone que el camino es válido respecto de la jerarquía dada. Si esto no sucede debe fallarse con un error adecuado.
-}
subCategoriasDe :: CaminoJ -> MVTree -> [Categoria] -- O(J*(log SC) + SC) por cada categoria del camino jerarquico se hace una operación logaritmica, luego la suma de cantidad de subcategorías de la categoría final del camino
--lookupM = O(log SC) donde SC es la cantidad de subcategorias de una jerarquía de categoría
--domM = O(SC) donde SC es la cantidad total de subcategorias restantes
subCategoriasDe [] (MVT hip mcm)     = domM mcm
subCategoriasDe (c:cs) (MVT hip mcm) = case (lookupM c mcm) of
                                    Nothing   -> error "La subcategoria no existe"
                                    Just mvt' -> subCategoriasDe cs mvt'



{-
que dado un producto, un camino jerárquico y una jerarquía de categorías, describe la jerarquía que resulta de agregar una venta del producto en cada una de las categorías del camino. Se supone que el camino es válido respecto de la jerarquía dada. Si esto no sucede debe fallarse con un error adecuado
-}
registrarVenta :: Producto -> CaminoJ -> MVTree -> MVtree --O(J*(log SC + P log P)) donde J es la longitud del camino jerarquico dado
--registrarVentaH = O(P log P)
--lookupM = O(log SC) donde SC es la cantidad de subcategorías del map
--assocM = O(log SC) donde SC es la cantidad de subcategorías del map
registrarVenta p [] (MVT hip mcm)     = MVT (registrarVentaH p hip) mcm
registrarVenta p (c:cs) (MVT hip mcm) = case (lookupM c mcm) of
                                        Nothing   -> error "La categoría no existe"
                                        Just mvt' -> MVT (registrarVentaH p hip) (assocM c (registrarVenta p cs mvt') mcm)

registrarVentaH :: Producto -> Heap (Int, Producto) -> Heap (Int, Producto) --O(P log P) Por cada producto P hace una operación logaritmica que en el peor caso es la cantidad de productos de toda la heap
--isEmptyH = O(1)
--findMaxH = O(1)
--insertH = O(log P) donde P es la cantidad de productos de la heap
--deleteMax = O(log P) donde P es la cantidad de productos de la heap
registrarVentaH p hip = if isEmptyH hip
                        then insertH (1, p) emptyH
                        else let (n, p') = findMaxH hip in
                            if (p==p')
                            then insertH (n+1,p') (deleteMax hip)
                            else insertH (n, p') (registrarVentaH p (deleteMax hip))



{-
que dado un número n, un caminoJ, y una jerarquía de categorías, describe la lista de los n productos más vendidos en la categoría final del camino. Se supone que el camino es válido respecto de la jerarquía dada, si esto no sucede debe fallarse con un error adecuado.
-}
masVendidosEn :: Int -> CaminoJ -> MVtree -> [Productos] -- O(J*(log SC) + P log P) por cada categoria J hago una operación logaritmica, más la suma
--lookupM = O(log SC) donde SC es la cantidad de subcategorias del map
--productosMasVendidos = O(P log p)
masVendidosEn n [] (MVT hip mcm)     = productosMasVendidos n hip
masVendidosEn n (c:cs) (MVT hip mcm) = case (lookupM c mcm) of 
                                        Nothing   -> error "La categoría no existe"
                                        Just mvt' -> masVendidosEn n cs mvt'

productosMasVendidos :: Int -> Heap (Int, Producto) -> [Producto] --En peor caso : O(P*(log P)) donde P es la cantidad de iteraciones de productos que en peor caso puede ser toda la heap
--isEmptyH = O(1)
--findMaxH = O(1)
--deleteMaxH = O(log P) donde P es la cantidad de productos de la heap
productosMasVendidos 0 _   = []
productosMasVendidos n hip = if isEmptyH hip
                            then []
                            else let (i, p) = findMaxH hip in
                                p : productosMasVendidos (n-1) (deleteMaxH hip)


{-
Ejercicio 1) implementar la función esMasVendido:: Producto -> CaminoJ -> Int -> MVTree -> Boolean
que dado un producto indica si el producto está entre los n más vendidos de alguna de las categorías del camino, Se supone que el camino es válido respecto a la jerarquía dada. Si esto no sucede, debe fallarse con un error adecuado

Ayuda: es posible que resulte muy útil usar sin definir la siguiente función que calcula todos los segmentos iniciales de una lista, cuya eficiencia es O(n^2) siendo n la longitud de la lista

inits :: [a] -> [[a]]
Ejemplo: inits [1,2,3,4] = [[],[1],[1,2],[1,2,3],[1,2,3,4]]
-}

esMasVendido :: Producto -> CaminoJ -> Int -> MVTree -> Bool
esMasVendido p cj n mvt = esMasVendidoEn p n (inits cj) mvt

esMasVendidoEn :: Producto -> Int -> [CaminoJ] -> MVTree -> Bool -- O(J*(P + J*(log SC + P log P)))
--elem = O(P) donde P es la cantidad de productos más vendidos de la lista
--masVendidoEn = O(J*(log SC) + P log P)
esMasVendidoEn p n [] mvt       = False
esMasVendidoEn p n (cj:cjs) mvt = elem p (masVendidosEn n cj mvt) || esMasVendidoEn p n cjs mvt