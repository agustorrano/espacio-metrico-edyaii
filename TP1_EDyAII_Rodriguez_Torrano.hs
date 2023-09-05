
--  TRABAJO PRÁCTICO 1 EDyAII
--  Nerina Rodriguez Chybik y María Agustina Torrano
--  15/05/2023

-- 1)
-- a)
data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty deriving (Eq, Ord, Show)

class Punto p where
            dimension :: p -> Int
            coord :: Int -> p -> Double
            dist :: p -> p -> Double
            dist p1 p2 = sum [((coord i p1) + (coord i p2))^2 | i <- [0..((dimension p1) - 1)]]

-- b)
newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)
newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

instance Punto Punto2d where
        dimension _ = 2
        coord 0 (P2d (x, y)) = x
        coord 1 (P2d (x, y)) = y

instance Punto Punto3d where
        dimension _ = 3
        coord 0 (P3d (x, y, z)) = x
        coord 1 (P3d (x, y, z)) = y
        coord 2 (P3d (x, y, z)) = z

-- 2)
{-
 - Dado un punto p y un nivel (nivel del árbol), 
 - la función calcula el eje que debe tener el punto p
 -}
eje :: Punto p => p -> Int -> Int
eje p n = mod n (dimension p)

{-
 - Dada una lista de puntos y un eje, la función retorna una
 - lista con los puntos ordenados de menor a mayor
 -}
sort :: Punto p => [p] -> Int -> [p]
sort [] _ = []
sort (x:xs) e = (sort smaller e) ++ [x] ++ (sort larger e)
                where smaller = [a | a <- xs, (coord e a) <= (coord e x)]
                      larger = [b | b <- xs, (coord e b) > (coord e x)]

{-
 - Dada una lista de puntos, un eje e, la mediana original de la lista
 - (div largo_lista 2) y el largo de la lista, retorna la mediana, teniendo
 - en cuenta el caso que haya algun punto distinto a la mediana, pero que 
 - tiene la coordenada e igual a la misma
 -}
mediana :: Punto p => [p] -> Int -> Int -> Int -> Int
mediana xs e m l = if (m+1) < l && coord e (xs !! m) == coord e (xs !! (m+1)) then mediana xs e (m+1) l else m

{-
 - Función auxiliar para la función fromList que, además de la lista
 - de puntos, toma el nivel del árbol
 -}
fromListAux :: Punto p => [p] -> Int -> NdTree p
fromListAux [] _ = Empty
fromListAux [x] n = Node Empty x Empty (eje x n)
fromListAux xs n = let e = eje (head xs) n
                       listaOrd = sort xs e
                       largo = length listaOrd
                       indMed = mediana xs e (div largo 2) largo
                       med = listaOrd !! indMed
                       hijosIzq = take indMed listaOrd
                       hijosDer = drop (indMed + 1) listaOrd
                       in Node (fromListAux hijosIzq (n + 1)) med (fromListAux hijosDer (n + 1)) e

fromList :: Punto p => [p] -> NdTree p
fromList xs = fromListAux xs 0

-- 3)
{-
 - Función auxiliar para la función insertar que, además del
 - punto a agregar y el árbol, toma el nivel del árbol
 -}
insertarAux :: Punto p => p -> NdTree p -> Int -> NdTree p
insertarAux p Empty n = Node Empty p Empty (eje p n)
insertarAux p (Node l q r e) n = if (coord e p) <= (coord e q) then (Node (insertarAux p l (n+1)) q r e) else (Node l q (insertarAux p r (n+1)) e)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p t = insertarAux p t 0

-- 4)
{-
 - Dado un árbol y un eje e, retorna el punto del árbol cuya 
 - coordenada e es mínima
 -}
minTree :: Punto p => NdTree p -> Int -> p
minTree (Node Empty p Empty e) _ = p
minTree (Node Empty p r e) eje = if e == eje then p
                                   else let x = minTree r eje
                                        in if coord eje p >= coord eje x then x else p
minTree (Node l p Empty e) eje = if e == eje then minTree l eje
                                   else let x = minTree l eje
                                        in if coord eje p >= coord eje x then x else p
minTree (Node l p r e) eje = if e == eje then minTree l eje
                               else let x = minTree l eje
                                        y = minTree r eje
                                    in if coord eje x <= coord eje p && coord eje x <= coord eje y then x
                                       else if coord eje y <= coord eje p && coord eje y <= coord eje x then y
                                       else p

{-
 - Dado un árbol y un eje e, retorna el punto del árbol cuya 
 - coordenada e es máxima
 -}
maxTree :: Punto p => NdTree p -> Int -> p
maxTree (Node Empty p Empty e) _ = p
maxTree (Node Empty p r e) eje = if e == eje then maxTree r eje
                                   else let x = maxTree r eje
                                        in if coord eje p <= coord eje x then x else p
maxTree (Node l p Empty e) eje = if e == eje then p
                                   else let x = maxTree l eje
                                        in if coord eje p <= coord eje x then x else p
maxTree (Node l p r e) eje = if e == eje then maxTree r eje
                               else let x = maxTree l eje
                                        y = maxTree r eje
                                    in if coord eje x >= coord eje p && coord eje x >= coord eje y then x
                                       else if coord eje y >= coord eje p && coord eje y >= coord eje x then y
                                       else p

-- Dado un árbol, reemplaza la raíz según la implementacón dada
reemplazo :: (Eq p, Punto p) => NdTree p -> NdTree p
reemplazo (Node l _ r e) = if r /= Empty 
                           then let remp = minTree r e
                                in Node l remp (eliminar remp r) e
                           else let remp = maxTree l e
                                in Node (eliminar remp l) remp r e

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p t1@(Node Empty q Empty e) = if p == q then Empty else t1
eliminar p t2@(Node l q r e) | p == q = reemplazo t2
                             | coord e p <= coord e q = Node (eliminar p l) q r e
                             | otherwise = Node l q (eliminar p r) e

-- 5)
-- a)
type Rect = (Punto2d, Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x, y)) (P2d (x1, y1), P2d (x2, y2)) = x <= max x1 x2 &&
                                                     x >= min x1 x2 &&
                                                     y <= max y1 y2 &&
                                                     y >= min y1 y2

-- b)
{-
 - Dado un rectángulo, pone el mínimo x y el mínimo y en la primer
 - componente del rectángulo, y el máximo x y el máximo y en la 
 - segunda componente
 -}
rectMinMax :: Rect -> Rect
rectMinMax (P2d (x1, y1), P2d (x2, y2)) = let minX = min x1 x2
                                              minY = min y1 y2
                                              maxX = max x1 x2
                                              maxY = max y1 y2
                                          in (P2d (minX, minY), P2d (maxX, maxY))

{-
 - Función auxiliar para ortogonalSearch, la cual tiene en cuenta que
 - la primer componente del rectángulo está conformado por los valores
 - mínimos, y la segunda por los máximos
 -}
ortogonalSearchAux :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearchAux Empty rect = []
ortogonalSearchAux (Node Empty p Empty e) rect = [p | inRegion p rect]
ortogonalSearchAux (Node l p r e) (r1, r2) = let izqList = if coord e p < coord e r1 then [] 
                                                           else ortogonalSearchAux l (r1, r2)
                                                 derList = if coord e p > coord e r2 then []
                                                           else ortogonalSearchAux r (r1, r2)
                                             in izqList ++ [p | inRegion p (r1, r2)] ++ derList

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch t rect = let r = rectMinMax rect
                         in ortogonalSearchAux t r
