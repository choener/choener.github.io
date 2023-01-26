
{-# Options_GHC -fenable-rewrite-rules #-}
{-# Options_GHC -ddump-rule-firings #-}

-- | Pearls of Functional Algorithm Design, (7)
-- Build a tree with minimum height

module Trees where
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)


-- * Gegeben ist eine Liste von Gewichten @xs = [x1, ..., xn]@, finde Baum, dessen "fringe" gleich @xs@ ist. Dieser Baum soll minimale Kosten folgendemassen haben: die Kosten in den Blaettern ergeben sich aus dem fringe; die Kosten eines Forks sind 1+max l r, wobei l,r die Teilbaeume sind.

-- | Baeume mit Information in den Blaettern.

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Eq,Ord,Show)

-- | Waelder sind Listen von Baeumen.

type Forest a = [Tree a]

-- | Kostenfunktion die zu minimieren ist. Blaetter haben Kosten @x@, Innere Knoten haben Kosten 1+
-- das Maximum ueber links und rechts.

cost :: (Tree Int -> Int)
cost (Leaf x) = x
cost (Fork l r) = 1 + (cost l `max` cost r)

-- | Gegeben die Liste xs, generiere den Forest aller moeglichen Baeume. Diese Definition ist
-- rekursiv.

trees :: [Int] -> Forest Int
-- Eine 1-elementige Liste liefert einen Forest mit einem Blatt.
trees [x] = [Leaf x]
-- Eine vielelementige Liste generiert alle Baeume auf dem Tail. Fuer jeden solchen Baum wird der
-- head links in den linken spine an alle moeglichen Positionen gesetzt.
trees (x:xs) = concatMap (prefixes x) (trees xs)

-- | Konstruiere alle Prefixe mit @x@, fuer einen gegebenen Baum.

prefixes :: Int -> Tree Int -> [Tree Int]
-- Ein Leaf wird rechtes Blatt, um die fringe-property beizubehalten
prefixes x t@(Leaf y) = [Fork (Leaf x) t]
-- Ansonsten konstruiere den linken spine, die Anzahl bestimmt sich durch den Pfad von der Wurzel
-- zum Kind ganz links in l. (siehe Tafelbild)
prefixes x t@(Fork l r) = Fork (Leaf x) t : [Fork l' r | l' <- prefixes x l]

{-# RULES
  "mincostTree" mincostTree = mincostTree2
  #-}

-- | Gegeben irgendeine Liste, konstruiere alle Waelder mit gleicher fringe, berechne deren Kosten
-- und gebe das lexikographisch kleinste Element mit minimalen Kosten zurueck. Diese Funktion ist
-- exponentiell!
--
-- @mincostTree [1..k]@ braucht
--  - k=13 =  2s
--  - k=14 = 10s
--  - k=15 = 33s

mincostTree :: [Int] -> Tree Int
{-# Inline [1] mincostTree #-}
mincostTree = minimumBy (comparing cost) . trees



-- * Mehr Effizienz

-- | Vergleiche @foldr :: (a->b->b) -> b -> [a] -> b
--
-- Hier haben wir @x1 `f` x2 `f` x3 `f` ... `f` g x@

foldrn :: (a->b->b) -> (a->b) -> [a] -> b
-- Wenn [x] singulaer, dann @g x@
foldrn f g [x] = g x
-- Ansonsten @x `f` (foldrn f g xs)
foldrn f g (x:xs) = f x (foldrn f g xs)

-- | Rolle einen Wald in einen Baum auf.
-- @x1 `Fork` (x2 `Fork` X3)@:
-- @rollup [Leaf 1, Leaf 2, Leaf 3] = Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)@

rollup :: Forest a -> Tree a
rollup = foldl1 Fork

-- Fusion law for foldrn
-- @
-- forall xs, nonempty xs,
-- h (foldrn f g xs) = foldrn f' g' xs
-- gegeben das:
-- h (g x) = g' x
-- und
-- h (f x y) = f' x (h y)
-- @

-- | Konstruiere einen Forest, wobei @x@ der erste Baum wird (und nur aus einem Leaf besteht). Der
-- restliche Wald geht durch ein 'split'.

insert :: Int -> Forest Int -> Forest Int
insert x ts = Leaf x : split x ts

-- | 'split' wandelt den Forest so das der Baum @u@ oder @x@ guenstiger sind als @v@, oder wird den
-- Anfang @u,v@ solange als neuen Fork bauen, bis das stimmt.

split :: Int -> Forest Int -> Forest Int
split x [u] = [u]
split x (u:v:ts) = if x `max` cost u < cost v then u:v:ts
                   else split x (Fork u v:ts)

-- | == @(:[])@

wrap ::x -> [x]
wrap x = [x]

-- | Neue Konstruktion in quadratischer Zeit

mincostTree2 = foldl1 Fork . foldrn insert (wrap . Leaf)


-- * Lineare Zeit via "augmentation"

type Tree' = (Int, Tree Int)

-- | Nutze prime-Versionen, die die Kosten explizit annotieren

mincostTree3 :: [Int] -> Tree Int
mincostTree3 = foldl1 Fork . map snd . foldrn insert' (wrap . leaf')

-- | Insert, aber konstruiere die Leafs zusammen mit Kosten

insert' :: Int -> [Tree'] -> [Tree']
insert' x ts = leaf' x : split' x ts

-- | Split rechnet jetzt nicht mit Kosten, sondern nimmt die Augmentation.

split' :: Int -> [Tree'] -> [Tree']
split' x [u] = [u]
split' x (u:v:ts) = if x `max` fst u < fst v then u : v : ts
                    else split' x (fork' u v : ts)

-- | Augmentation mit Leaf: smart constructor

leaf' x = (x, Leaf x)

-- | Fork, mit Augmentation als smart constructor.

fork' (a,u) (b,v) = (1+ a `max` b, Fork u v)

test = mincostTree

