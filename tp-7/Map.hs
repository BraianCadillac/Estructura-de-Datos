module Map(Map, emptyM, assocM, lookupM, deleteM, keys) where

{-

Dada la siguiente interfaz y costos para el tipo abstracto Map:

emptyM :: Map k v
Costo: O(1).

assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).

lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).

deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).

keys :: Map k v
Costo: O(K).
-}

data Tree k v = EmptyT | NodeT k v (Tree k v) (Tree k v)
    deriving Show

data Map k v = M (Tree k v)
    {-
    INV REPR: En (M t)
            *En t no hay claves repetidas
            *t es un BST
    -}

emptyM :: Map k v --O(1)
emptyM = M (EmptyT)

assocM :: Ord k => k -> v -> Map k v -> Map k v --O(log K) por asociar
assocM k v (M t) = M (asociar k v t)

asociar :: Ord k => k -> v -> Tree k v -> Tree k v --O(log K) siendo k la cantidad de claves en el árbol
asociar k v EmptyT              = NodeT k v EmptyT EmptyT
asociar k v (NodeT k' v' ti td) = if (k == k') 
                                    then NodeT k v ti td
                                    else if (k < k')
                                        then NodeT k' v' (asociar k v ti) td
                                        else NodeT k' v' ti (asociar k v td)

lookupM :: Ord k => k -> Map k v -> Maybe v --O(log K) por lookupM'
lookupM k (M t) = lookupM' k t

lookupM' :: Ord k => k -> Tree k v -> Maybe v --O(log K) siendo K la cantidad de claves en el árbol
lookupM' k EmptyT             = Nothing
lookupM' k (NodeT k' v ti td) = if (k == k')
                                then Just v
                                else if (k < k')
                                    then (lookupM' k ti) 
                                    else (lookupM' k td)


deleteM :: Ord k => k -> Map k v -> Map k v
deleteM k (M t) = M (deleteT k t)

deleteT :: Ord k => k -> Tree k v -> Tree k v
deleteT k EmptyT = EmptyT
deleteT k (NodeT k' v ti td) =
  if k == k'
    then borrarRaiz (NodeT k' v ti td)
    else if k < k'
      then NodeT k' v (deleteT k ti) td
      else NodeT k' v ti (deleteT k td)

borrarRaiz :: Ord k => Tree k v -> Tree k v
borrarRaiz (NodeT _ _ EmptyT td) = td
borrarRaiz (NodeT _ _ ti EmptyT) = ti
borrarRaiz (NodeT _ _ ti td) =
  let ((k,v), td') = splitMin td
  in NodeT k v ti td'

splitMin :: Tree k v -> ((k,v), Tree k v)
splitMin (NodeT k v EmptyT td) = ((k,v), td)
splitMin (NodeT k v ti td) =
  let (minKV, ti') = splitMin ti
  in (minKV, NodeT k v ti' td)

keys :: Map k v -> [k] --Costo: O(K) = siendo K todas las claves del map 
keys (M t) = clavesDe t

clavesDe :: Tree k v -> [k] -- O(K) siendo K todas las claves del árbol
clavesDe EmptyT            = []
clavesDe (NodeT k v ti td) = k : clavesDe ti ++ clavesDe td

