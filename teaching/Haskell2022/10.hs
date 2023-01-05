{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Ten where

-- Basis
import Data.List
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as M
import Text.Printf
-- State
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State.Strict
-- ANSI Grafik
import Data.Colour.RGBSpace
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI as C
import System.IO (hFlush, stdout, hSetEcho)



-- * Die Klasse generischer Puzzles (Rushhour, Schach, Go, etc ...)

-- | Puzzles die durch 'moves' geloest werden koennen und sich durch States zwischen den Moves
-- auszeichnen.

class Puzzle p where
  -- | Der aktuelle Zustand des Puzzles
  data PState p :: *
  -- | Ein Schritt zwischen zwei Zustaenden
  data Move p :: *
  -- | Alle moves von einem State aus
  moves :: PState p -> [Move p]
  -- | State nach State mittels Move
  move :: PState p -> Move p -> PState p
  -- | Ist das Problem geloest?
  solved :: PState p -> Bool

-- | Pfade die zu einem State fuehren
type Path p = ([Move p], PState p)
-- | Die "Grenze" sind alle bisher erkundeten Pfade, die potentiell noch zum Ziel fuehren koennen.
type Frontier p = [Path p]

-- | Groesse der aktuellen Frontier

newtype FrontierSz = FSz Int
  deriving (Eq,Ord,Show,Num)

-- | @search@ erlaubt BFS und DFS Suchen.

data SearchTy = BFS | DFS
  deriving (Eq,Ord,Show)

-- | Gegeben einen 'Path', berechne alles 'moves' von @q@ aus, wodurch eine Liste von 'Path'
-- entsteht. Jeweils mit einem eigenem State, dieser wurde durch 'move' erreicht.

succs :: Puzzle p => Path p -> [Path p]
succs (ms,q) = [(ms ++ [m], move q m) | m <- moves q]

-- | Entweder BFS oder DFS
--
-- TODO Nutze nicht [] sondern eine andere Struktur um "length" guenstiger zu machen!

search :: (Monad m, MonadState FrontierSz m, Puzzle p, Eq (PState p))
  => SearchTy     -- ^ DFS oder BFS
  -> [PState p]   -- ^ alle bisher besuchten States
  -> Frontier p   -- ^ momentan aktive Pfade die zum Ziel fuehren koennten
  -> m (Maybe [Move p])
-- Keine Frontier mehr zu testen, keine Lsg
search sty qs [] = pure Nothing
-- Potentielle Lsg'en
search sty qs (p@(ms,q):ps)
-- wir haben die erste Lsg gefunden
  | solved q    = pure $ Just ms
-- "q" ist schon Teil elaborierter State's
  | q `elem` qs = search sty qs ps
-- teste weitere Pfade
  | otherwise   = do
      let xs = succs p
      modify (+1)
      search sty (q:qs) $ case sty of
        BFS -> ps ++ xs -- erst schon aktive Pfade, dann tiefer gehen
        DFS -> xs ++ ps -- erst tiefer gehen, dann schon aktive Pfade

-- | Wir koennen Constraints auch zusammen fassen
type SolveTy m p = (Monad m, MonadState FrontierSz m, Puzzle p, Eq (PState p))

-- | Loese ein Puzzle Problem durch Wahl der Suchmethode und des Startzustandes.

solve :: SolveTy m p => SearchTy -> PState p -> m (Maybe [Move p])
solve sty q = search sty [] [([],q)]

-- | Standard-Runner, extrahiert die Groesse der Grenze

runSolve sty q = runState (solve sty q) (FSz 0)



-- * Loesung fuer Rushhour !

-- | Name des Spiels

data Rushhour = Rushhour
  deriving (Show)

-- | Zellen sind einfach 'Int's
type Cell = Int
-- | Grid ist eine Liste von Zell-paaren fuer erste, letzte Zelle
type Grid = [(Cell,Cell)]
-- | Auto-Index
newtype VehicleIx = VIx Int
  deriving (Eq,Ord,Show,Enum,Num)

-- | Gegeben ein Gridd, welche Zellen sind belegt?
occupied :: Grid -> [Cell]
occupied = concatMap fillcells

-- | Autos entweder horizontal @(3,4)@ oder vertical @(3,10)@; potentiell auch 3, statt 2 Zellen
-- lang
fillcells (r,f) = if r > f-7 then [r..f] else [r,r+7..f]

-- | Ist ein Auto horizontal?
isHoriz (r,f) = r>f-7

-- | Transformation von Grid nach x,y Koordinaten
toXY = map ((\(l,r) -> (l+1,r)) . (`divMod` 7)) . fillcells

-- | Welche Zellen sind nicht belegt?
freecells :: Grid -> [Cell]
freecells g = allcells \\ occupied g

-- | Alle Zellen des Spielfeldes
allcells :: [Cell]
allcells = [ c | c <- [1..41], c `mod` 7 /= 0 ]

-- | Gegeben ein Auto, was sind die benachbarten Zellen, potentiell ausserhald des Spielfeldes!
adjs :: (Cell,Cell) -> [Cell]
adjs (r,f) = if r > f-7 then [f+1,r-1] else [f+7,r-7]

-- | Bewege ein Auto einen Schritt.
adjust :: (Cell,Cell) -> Cell -> (Cell,Cell)
adjust (r,f) c
  -- horizontale Autos nach rechts oder links
  | r > f-7 = if c > f then (r+1,c) else (c,f-1)
  -- vertikale Autos nach oben oder unten
  | otherwise = if c < r then (c,f-7) else (r+7,c)

-- | Loese Rushhour, indem eine Instanz von Puzzle geschrieben wird
instance Puzzle Rushhour where
  -- | Der State ist durch das jeweils aktive Grid gegeben
  newtype PState Rushhour = Rstate { rstate :: Grid }
    deriving (Eq)
  -- | Ein Move schiebt ein Auto, wir brauchen den Index des Autos und die Ziel-Zelle
  newtype Move Rushhour = Rmove { rmove :: (VehicleIx,Cell) }
    deriving (Show)
  -- | Legale Moves, gegeben einen State
  moves :: PState Rushhour -> [Move Rushhour]
  moves (Rstate g) = let fs = freecells g
    in  [ Rmove (v,c) | (v,i) <- zip [0..] g, c <- adjs i, c `elem` fs ]
  -- Ein move ist ein Update von genau einem Auto via Index
  move :: PState Rushhour -> Move Rushhour -> PState Rushhour
  move (Rstate g) (Rmove (VIx v,c)) = let (g1,i:g2) = splitAt v g
    in Rstate $ g1 ++ adjust i c : g2
  -- Teste ob das Rechte Ende vom Auto in der End-Zelle steht
  solved :: PState Rushhour -> Bool
  solved (Rstate g) = snd (head g) == 20

glol :: PState Rushhour
glol = Rstate [ (17,18), (1,9) ]

-- | Problem von der Webseite

g0 :: PState Rushhour
g0 = Rstate [ (16,17), (1,15), (2,3), (11,25), (22,29), (30,31), (38,40), (27,41) ]

-- Problem aus dem Buch

g1 :: PState Rushhour
g1 = Rstate [ (17, 18), (1, 15), (2, 9), (3, 10), (4, 11), (5, 6), (12, 19)
            , (13, 27), (24, 26), (31, 38), (33, 34), (36, 37), (40, 41)]

g2 :: PState Rushhour
g2 = Rstate [ (18,19), (1,15), (2,3), (5,12), (9,16), (10,17), (13,27)
            , (22,24), (25,32), (31,38), (33,34), (36,37), (39,40) ]

-- | Plot moves, bischen Spass mit ANSI

plotMoves :: SearchTy -> PState Rushhour -> IO ()
plotMoves sty grid = do
  setSGR [SetColor Foreground Dull Blue]
  print (rstate grid)
  before <- getCurrentTime
  let solu = runSolve sty grid
  after <- snd solu `seq` getCurrentTime
  printf "This took %s seconds\n" (show $ diffUTCTime after before)
  case runSolve sty grid of
    (Nothing, FSz depth) -> do
      setSGR [SetColor Foreground Vivid Red]
      printf "no solution with final frontier size: %d\n" depth
    (Just sol, FSz depth) -> do
      setSGR [SetColor Foreground Vivid Blue]
      printf "found solution with frontier size: %d\n" depth
      let is = scanl move grid sol
      hSetEcho stdin False
      whileGrid is 0
      hSetEcho stdin True
  setSGR [Reset]  -- reset to default colour scheme

-- | Bis wir @q@ druecken, koennen wir uns alle Zuege nacheinander ansehen

whileGrid :: [PState Rushhour] -> Int -> IO ()
whileGrid is k = do
  printf "%5d of %5d\n" (k+1) (length is)
  plotGrid $ is !! k
  cursorUp 13
  c <- getChar
  if | c == 'q' -> cursorDown 13 >> pure ()
     | c == 'p' -> whileGrid is (max 0 $ k-1)
     | c == 'P' -> whileGrid is (max 0 $ k-25)
     | c == 'n' -> whileGrid is (min (k+1) $ length is - 1)
     | c == 'N' -> whileGrid is (min (k+25) $ length is - 1)
     | otherwise -> whileGrid is k

-- plot a Pstate
--
-- TODO getc, q,<-,-> or q,p,n

plotGrid :: PState Rushhour -> IO ()
plotGrid g = do
  let occ = occupied (rstate g)
      (x,y) = head (rstate g)
      colors :: M.Map Int [SGR]
      colors = M.fromList . concatMap (\(c,l) -> map (,l) c) $ zip (map fillcells $ rstate g) (cycle others)
      pix sgr i = setSGR [Reset] >> putStr " " >> setSGR sgr >> printf "%2d" i
  forM_ [1..6] $ \r -> do
    forM_ [1..6] $ \c -> do
      let ix = (r-1)*7 + (c::Int)
      if | ix `elem` [x,y] -> do
            pix [SetColor Foreground Vivid Red, SetColor Background Vivid White] ix
         | Just clr <- colors M.!? ix -> do
            pix clr ix
         | otherwise -> pix [SetColor Foreground Dull White] ix
    setSGR [Reset]
    putStrLn "\n"
  setSGR [Reset]

others = [ [SetPaletteColor Background fg, SetColor Foreground Dull Black]
         | fg <- [10, 20 .. 255]
         ]

