module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node t) = S.fromList [t]
nodes (Overlay x y) = S.union (nodes x) (nodes y)
nodes (Connect x y) = S.union (nodes x) (nodes y)
{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty;
edges (Node t) = S.empty;
edges (Overlay x y) = S.union (edges x) (edges y);
edges (Connect x y) = S.union (S.union (edges x) (edges y)) (S.cartesianProduct (nodes x) (nodes y) )

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
--outNeighbors node graph = 
outNeighbors t Empty = S.empty
outNeighbors t (Node f) = S.empty
outNeighbors t (Overlay x y)
    | S.member t (nodes x) == True && S.member t (nodes y) == False = outNeighbors t x
    | S.member t (nodes y) == True && S.member t (nodes x) == False = outNeighbors t y
    | S.member t (nodes x) == True && S.member t (nodes y) == True = S.union (outNeighbors t x) (outNeighbors t y)
outNeighbors t (Connect x y)
    | S.member t (nodes x) == True = S.union (nodes y) (outNeighbors t x)
    | S.member t (nodes y) == True = outNeighbors t y

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node x) = S.empty
inNeighbors node (Overlay x y)
    | S.member node (nodes x) == True && S.member node (nodes y) == False = inNeighbors node x
    | S.member node (nodes y) == True && S.member node (nodes x) == False = inNeighbors node y
    | S.member node (nodes x) == True && S.member node (nodes y) == True = S.union (inNeighbors node x) (inNeighbors node y)
inNeighbors node (Connect x y)
    | S.member node (nodes x) == True = inNeighbors node x
    | S.member node (nodes y) == True = S.union (nodes x) (inNeighbors node y)
{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
--removeNode node graph = 
removeNode node Empty = Empty
removeNode node (Node x) = if node == x then Empty else Node x
removeNode node (Overlay x y) = Overlay (f x) (f y)
    where f = removeNode node
removeNode node (Connect x y) = Connect (f x) (f y)
    where f = removeNode node

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
--splitNode old news graph = 
splitNode old news Empty = Empty
splitNode old news (Node x) = if x == old then foldl (\acc t-> Overlay acc t) Empty (map (\t -> Node t) news) else Node x
splitNode old news (Overlay x y) = Overlay (f x) (f y)
    where f = splitNode old news
splitNode old news (Connect x y) = Connect (f x) (f y)
    where f = splitNode old news

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
--mergeNodes prop node graph = undefined

mergeNodes prop node Empty = Empty
mergeNodes prop node (Node x) = if (prop x) == True then (Node node) else (Node x)
mergeNodes prop node (Overlay x y) = Overlay (f x) (f y)
    where f = mergeNodes prop node
mergeNodes prop node (Connect x y) = Connect (f x) (f y)
    where f = mergeNodes prop node
