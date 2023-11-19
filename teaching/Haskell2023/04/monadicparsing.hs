
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
{-# Language DeriveFunctor #-}


import Data.List (sort,subsequences,(\\))
import Data.Char (isDigit,isSpace,ord,toUpper,toLower)
import Text.Printf (printf)
import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))

main :: IO ()
main = do
  print "Give me numbers"
  ns :: [Int] <- sort . map read . words <$> getLine
  print "Give me a target"
  tgt :: Int <- fmap read getLine
  printf "Given numbers %s get closest to the target number %d with an arithmetic expression:\n" (unwords $ fmap show ns) tgt
  suggest :: String <- getLine
  let (expr,nst) = nearest tgt . concatMap mkExprs $ subseqs ns
      myexp = case runParser exprP suggest of
                Right a -> a
                Left e -> error e
  print myexp
  printf "You have: %d\n" $ auswerten myexp
  print expr
  printf "Optimal is: %d\n" nst



-- | Datentyp for die legalen Operatoren

data Op = Add | Sub | Mul | Div
  deriving (Show,Eq,Read,Bounded,Enum)

-- | Die Form von Countdown Ausdruecken

data Expr
  = Zahl Int
  | App Op Expr Expr
  deriving (Show,Read,Eq)

-- | Alias fuer den Wert eines Countdowns

type Wert = Int

-- | Gegeben Op und zwei Werte, rechne deren Ergebnis

appOp :: Op -> Wert -> Wert -> Wert
appOp Add l r = l + r
appOp Sub l r = l - r
appOp Mul l r = l * r
appOp Div l r = l `div` r

-- | Ausdruck auswerten

auswerten :: Expr -> Wert
auswerten (Zahl k) = k
auswerten (App o l r) = appOp o (auswerten l) (auswerten r)

-- | Ist eine Operation legal?

legal :: Op -> Wert -> Wert -> Bool
legal Add l r = True
legal Sub l r = r < l
legal Mul l r = True
legal Div l r = l `mod` r == 0

-- * Countdown (``AI'' version)

-- | All moeglichen Teilsequenzen einer Liste

subseqs :: [a] -> [[a]]
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
  where xss = subseqs xs

-- | Given ordered @xs@, create two ordered sublists, which when merged are equal to @xs@.
--
-- let (ys,zs) = unmerges xs
-- forall y in ys . z in zs : merge y z == xs

unmerges :: Show a => [a] -> [([a],[a])]
unmerges [x,y] = [([x],[y]), ([y],[x])]
unmerges (x:xs) = [([x],xs),(xs,[x])] ++ concatMap (add x) (unmerges xs)
  where add x (ys,zs) = [(x:ys,zs),(ys,x:zs)]
unmerges burn = error $ show burn


-- | Combine two expression trees with all possible, legal operations

combine :: (Expr,Int) -> (Expr,Int) -> [(Expr,Int)]
combine (l,v) (r,w) = [(App op l r, appOp op v w) | op <- ops, legal op v w]
  where ops = [Add,Sub,Mul,Div] -- [minBound..maxBound]

-- | Generate all possible expressions for our set of numbers

mkExprs :: [Int] -> [(Expr, Int)]
mkExprs [x] = [(Zahl x, x)]
mkExprs xs = [ ev | (ys,zs) <- unmerges xs
                  , l <- mkExprs ys, r <- mkExprs zs
                  , ev <- combine l r ]


nearest :: Int -> [(Expr,Int)] -> (Expr,Int)
nearest n ((e,v):evs)
-- direkt eine Loesung gefunden?
  | d == 0 = (e,v)
-- nein? Suche starten, mit Abstand d
  | otherwise = search n d (e,v) evs
  where d = abs (n-v)

search :: Int -> Int -> (Expr,Int) -> [(Expr,Int)] -> (Expr,Int)
-- es gibt nur suboptimale Lsg
search n d ev [] = ev
-- teste naechste Lsg
search n d ev ((e,v):evs)
-- optimal
  | d' == 0 = (e,v)
-- besser
  | d' < d  = search n d' (e,v) evs
-- schlechter
  | d' >= d = search n d ev evs
  where d' = abs (n-v)



-- * Tokenization

data Token
  = TNum Int | TOp Op | TLeft | TRight
  deriving (Show,Eq)

-- * Turn the input into tokens, these are more easy to handle

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
  | isDigit x = let (ls,rs) = span isDigit xs
                in  TNum (read $ x:ls) : tokenize rs
  | x `elem` "+-*/" = TOp (parseOp x) : tokenize xs
  | x == '(' = TLeft : tokenize xs
  | x == ')' = TRight : tokenize xs

parseOp :: Char -> Op
parseOp '+' = Add
parseOp '-' = Sub
parseOp '*' = Mul
parseOp '/' = Div

-- | Given user input as a string, say @(1+2)*3 - (8/4)@ return the correct 'Expr'.
--
-- This version performs no error checking of any kind.

-- turn into expr, by following order of precedence of operations with splitting!

token2Expr :: [Token] -> Expr
token2Expr xs = case pSumPNP xs
  of Just (expr,[]) -> expr
     Nothing -> error $ show xs

-- | Parse Numbers and parentheses with highest priority

pNumParen :: [Token] -> Maybe (Expr,[Token])
pNumParen (TNum n:xs) = Just (Zahl n, xs)
pNumParen (TLeft:xs) = case pSumPNP xs of
  Just (expr, TRight:ys) -> Just (expr, ys)
  Just _ -> Nothing -- misses clothing bracket
  Nothing -> Nothing
pNumParen _ = Nothing

-- | Parse products with next highest priority

pProdNP :: [Token] -> Maybe (Expr,[Token])
pProdNP xs = case pNumParen xs of
  Just (el, TOp Mul:ys) -> case pProdNP ys of
    Just (er, zs) -> Just (App Mul el er, zs)
    Nothing -> Nothing
  Just (el, TOp Div:ys) -> case pProdNP ys of
    Just (er, zs) -> Just (App Div el er, zs)
    Nothing -> Nothing
  res -> res

-- | Parse sums with lowest priority

pSumPNP :: [Token] -> Maybe (Expr,[Token])
pSumPNP xs = case pProdNP xs of
  Just (el, TOp Add:ys) -> case pSumPNP ys of
    Just (er, zs) -> Just (App Add el er, zs)
    Nothing -> Nothing
  Just (el, TOp Sub:ys) -> case pSumPNP ys of
    Just (er, zs) -> Just (App Sub el er, zs)
    Nothing -> Nothing
  res -> res


-- * Monadic combinator parsing

-- Define the *type* of parsers.
newtype Parser t a = Parser { parse :: [t] -> [(a,[t])] }

instance Show (Parser t a) where
  show (Parser f) = "i am a parser"


runParser :: (Show t, Show a) => Parser t a -> [t] -> Either String a
runParser p ts = case parse p ts of
  (a,[]):_ -> Right a
  [] -> Left "no input"
  ars -> Left $ show ars

itemP :: Parser t t
itemP = Parser go
  where go [] = []
        go (x:xs) = [(x,xs)]

atomP :: Eq t => t -> Parser t t
atomP c = Parser go
  where go [] = []
        go (x:xs) | x/=c = []
        go (x:xs) = [(x,xs)]

--class Functor f where
--  fmap :: (a->b) -> f a -> f b
--  fmap f cont = error "to implement"

machgross Nothing  = Nothing
machgross (Just a) = Just (toUpper a)

machgrossS [] = []
machgrossS (x:xs) = toUpper x : machgrossS xs

data MaybeMine a = NothingM | JustM a
  deriving (Functor,Show)

instance Functor (Parser t) where
  fmap :: (a -> b) -> Parser t a -> Parser t b
  fmap f (Parser p) = Parser (\cs -> [(f a,ds) | (a,ds) <- p cs])

instance Applicative (Parser t) where

  pure :: a -> Parser t a

  pure x = Parser (\cs -> [(x,cs)])

  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b

  Parser p <*> Parser q = Parser (\cs -> [ (f a,es) | (f,ds) <- p cs
                                         , (a,es) <- q ds])

-- |
-- @
-- empty <|> p === p
-- p <|> empty === p
-- p <|> (q <|> r) === (p <|> q) <|> r
-- @

instance {- Applicative (Parser t) => -} Alternative (Parser t) where
  empty = noP
  Parser p <|> Parser q = Parser $ \cs -> p cs ++ q cs

--instance (Monad (Parser t), Alternative (Parser t)) => MonadPlus (Parser t) where
--  mzero = empty
--  mplus = (<|>)

-- the monad laws must hold:
-- return a >>= f === f a
-- p >>= return === p
-- p >>= (\a -> (f a >>= g)) === (p >>= (\a -> f a)) >>= g
--
-- return is left and right unit for (>>=)    -- allow simplification
-- (>>=) is associative                       -- eliminate parentheses

instance Monad (Parser t) where
  return :: a -> Parser t a
  return = pure
  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  Parser p >>= pq = Parser (\cs -> [ (b,es) | (a,ds) <- p cs
                                   , let Parser q = pq a
                                   , (b,es) <- q ds])

add1P :: Char -> Parser Char Int
add1P a = Parser go
  where go [] = []
        go (x:xs) = [(ord x+ ord a,xs)]

noP :: Parser t a
noP = Parser $ \cs -> []

satP :: (t -> Bool) -> Parser t t

satP c = Parser go
  where go [] = []
        go (x:xs) | c x = [(x,xs)]
        go _ = []

--satP c = itemP >>= \x -> if c x then pure x else mzero
--
--satP c = do
--  x <- itemP
--  if c x then pure x else mzero
--
--satP c = Parser goL >>= \x -> if c x then Parser (\cs -> [(x,cs)]) else Parser (\cs -> [])
--  where goL [] = []
--        goL (x:xs) = [(x,xs)]

-- fuz rho DOh : do-notation

testPP =
  itemP >>= \x1 ->
  itemP >>= \x2 ->
  itemP >>
  itemP >>= \x4 ->
  return (x1,x2,x4)

testDO = do
  x1 <- itemP
  x2 <- itemP
  itemP
  x4 <- itemP
  return (x1,x2,x4)

readDO = do
  x1 <- getLine
  x2 <- getLine
  return (x1,x2)

lineDO = do { x1 <- itemP; x2 <- itemP; itemP; x4 <- itemP; return (x1,x2,x4) }

-- recursion combinators

theseP :: Eq t => [t] -> Parser t [t]
theseP [] = pure []
theseP (t:ts) = satP (t==) >> theseP ts
--theseP = mapM_ (satP . (==))
--theseP = foldr (\t -> (>>) (satP (t ==))) (pure ())

manyP :: Parser t a -> Parser t [a]
manyP p = someP  p <|> return []
someP p = do {x <- p; xs <- manyP p; return (x:xs)}

qqq :: Parser t a -> Parser t a -> Parser t a
qqq = (<|>)

-- example: 11123, parse '1', then parse the following '1's



-- extra

sepBy :: Parser t a -> Parser t b -> Parser t [a]
p `sepBy` s = (p `sepBy1` s) <|> return []

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
p `sepBy1` s = do {a <- p; as <- many (s >> p); return (a:as)}

bracketedP :: Parser t l -> Parser t x -> Parser t r -> Parser t x
bracketedP lP xP rP = do
  _l <- lP
  x  <- xP
  _r <- rP
  return x

chainl :: Parser t a -> Parser t (a -> a -> a) -> a -> Parser t a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl1 p op = p >>= go
  where go a = do
          f <- op
          b <- p
          go (f a b)
          <|> return a

-- lexical parsing

spaceP :: Parser Char String
spaceP = many (satP isSpace)

tokenP :: Parser Char a -> Parser Char a
tokenP p = p <* spaceP

stringP :: String -> Parser Char String
stringP = tokenP . theseP

-- finally the new expr parser, without tokenization

exprP :: Parser Char Expr
exprP = termP `chainl1` addopP

termP = factorP `chainl1` mulopP

factorP = numberP <|> bracketP

digitP :: Parser Char Int
digitP = satP isDigit >>= \x -> pure (ord x - ord '0')

numberP :: Parser Char Expr
numberP = do
  ds <- some digitP
  spaceP
  return . Zahl $ foldl (\acc x -> 10*acc + x) 0 ds

bracketP :: Parser Char Expr
bracketP = bracketedP l exprP r
  where l = tokenP $ atomP '('
        r = tokenP $ atomP ')'

addopP = (stringP "+" >> pure (App Add)) <|> (stringP "-" >> pure (App Sub))
mulopP = (stringP "*" >> pure (App Mul)) <|> (stringP "/" >> pure (App Div))



-- * Hausaufgaben

-- | Implementiere einen Parser für Floating-Point Zahlen, inklusive negativen Zahlen
-- "-3.1415"

floatingP :: Parser Char Double
floatingP = error "schreibe mich"

-- | Erweitere den Parser für floating-point Zahlen auf wissenschaftliche Notation
-- "-3.2e16", die obige Notation soll weitherhin funktionieren.

floatingEP :: Parser Char Double
floatingEP = error "schreibe mich"

-- | Dieser Parser soll sowohl ganze Zahlen, als auch Floating-Point Zahlen erkennen. Zusätzlich
-- soll der Rückgabe*typ* mir bereits sagen, welche Art von Zahl erkannt wurde.
-- Also "-3" ~> ganze Zahl
-- und "17e12" ~> floating point Zahl
--
-- Ihr muesst hier überlegen wie der Rückgabetyp aussehen soll.

zahlP :: Parser Char ( {- finde mich -} )
zahlP = error "schreibe mich"

-- | Let-Ausdruecke in Audruecken. Hier soll der Parser zum einen "let a = 2 in" - artige Ausdruecke
-- erlauben, aber auch Variablen in "a+b" zulassen.
--
-- Hierbei wird im *Parser* nicht geprueft, ob die Ausdruecke auch "Sinn" machen.
--
-- Welchen Type vergibt man hier? Muessen wir an anderer Stelle etwas erweitern?
--
-- "let a = 2 in a + a"
-- "1+(let a = 2 in a+3)"
-- "1+(let a= 2 in a+(let b = 3 in a+b))"
-- "let a = 2 in b+3" (auch im parser ok)

letP :: Parser Char ( {- finde mich -} )
letP = error "schreibe mich"

-- | Zum Nachdenken -- oder implementieren: Der monadische Parser nutzt intern die Datenstruktur
-- einer Liste @[(a,[s])]@ um anzuzeigen das @a@ erfolgreich geparsed wurde und die @[t]@ Token noch
-- ausstehen.
--
-- Allerdings kann ein parse natürlich auch komplett fehlschlagen. Dann haben wir intern @[]@ als
-- Resultat.
-- Allerdings wissen wir jetzt *nicht*, warum wir nicht parsen konnten, also was der Fehler ist.
--
-- 1) Wie sollte man Fehlerbehandlung durchführen? Was sind die Vor- und Nachteile verschiedener
-- Ideen? (Jetzt nicht als code, sondern als "grobe Idee")
--
-- 2) Im nächsten Schritt kann man jetzt fragen, wie man diese Ideen implementieren kann. Dazu erst
-- einmal: was sollte ein Fehlerbehandlungssystem können, *unabhängig* vom Parsen. Beispiel
-- @sichereDivision@, wie könnte so ein Typ aussehen?

sichereDivision :: Double -> Double -> ( {- Typ für Division mit Fehlerbehandlung -} )
sichereDivision = error "schreibe mich"

-- 3) Könnt ihr euch eine Abstraktion zu diesem "konkreten" Typ überlegen?
--
-- 4) Zurück zum Parsen: wie kann man Parsing und Fehlerbehandlung verheiraten?


