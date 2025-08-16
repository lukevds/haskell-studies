-- create function that filters tree for nodes that win for a player
-- then sort the tree by height, shortest do tallest
import Data.Char
import Data.List
import System.IO


isSomething :: Maybe a -> Bool
isSomething (Just _) = True
isSomething _        = False

takeJust :: Maybe a -> a
takeJust (Just x) = x


data Tree a = Node a [Tree a]
              deriving Show

getNode :: Tree a -> a
getNode (Node x _) = x

filterTree :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree f (Node a []) = if f (Node a []) then Just (Node a []) else Nothing
filterTree f (Node a as) =
  if f a then
    Just (Node a [takeJust d | d <- [filterTree f c | c <- as], isSomething d])
  else
    Nothing

sortByTree :: (Tree a -> Tree a -> Ordering) -> Tree a -> Tree a
sortByTree f (Node a []) = Node a []
sortByTree f (Node a as) = Node a (sortBy f as)

anyTree :: (a -> Bool) -> Tree a -> Bool
anyTree f (Node a as) = f a || any (anyTree f) as

treeHeight :: Tree a -> Int
treeHeight (Node a []) = 1
treeHeight (Node a (x:xs)) = 1 + (foldl max (treeHeight x) [treeHeight t | t <- xs])

sortTreeByHeight :: Tree a -> Tree a
sortTreeByHeight t = sortByTree f t
  where
    f a b = compare (treeHeight a) (treeHeight b)

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

size :: Int
size = 3

depth :: Int
depth = 9

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node x []) | wins O x  = Node (x,O) []
                    | wins X x  = Node (x,X) []
                    | otherwise = Node (x,B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax ts
      ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree

eventuallyWins :: Player -> Tree Grid -> Bool
eventuallyWins p tg = anyTree (\x -> wins p (getNode x)) tg
-- eventuallyWins :: Grid -> Player -> Bool
-- eventuallyWins g p = wins p g || willWin
--   where
--     tree    = gametree g p
--     willWin = 

winningGrids :: Player -> Tree Grid -> Maybe (Tree Grid)
winningGrids p gt = filterTree (eventuallyWins p) gt

bestmove' :: Grid -> Player -> Maybe Grid
bestmove' g p =
  case g' of
    Just ng ->
      case sorted of
        (Node c []) -> Just c
        (Node c cs) -> Just (getNode (head cs))
      where
        sorted = sortTreeByHeight ng
    Nothing -> Nothing
  where
    g' = winningGrids p (gametree g p)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
           os = length (filter (== O) ps)
           xs = length (filter (== X) ps)
           ps = concat g


wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..(size-1)]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It’s a draw!\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move"
                           run' g p
                  [g'] -> run g' (next p)


prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play' g p
                    [g'] -> play g' (next p)
  | p == X   = do putStr "Player X is thinking... "
                  (play $! (bestmove g p)) (next p)              

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O
