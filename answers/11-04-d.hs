import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

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

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

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
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
             where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g     = []
  | full g    = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 5

nextp :: Player -> Player
nextp O = X
nextp B = B
nextp X = O

data Core = Hum | Comp
            deriving (Eq, Show)

nextCore :: Core -> Core
nextCore Hum = Comp
nextCore Comp = Hum

getTurn :: IO Int
getTurn = do n <- getNat "Enter your turn (1 or 2): "
             if not (elem n [1,2]) then
                getTurn
             else
                return n

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          prepare

rmdups :: Ord a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

winnum :: Int
winnum = 3

wins :: Player -> Grid -> Bool
wins p g = any (line p) (rows ++ cols ++ dias)
           where
              rows = g
              cols = transpose g
              dias = diags g

line :: Player -> [Player] -> Bool
line p xs | length xs < winnum = False
          | all (==p) (take winnum xs) = True
          | otherwise = line p (drop 1 xs)

diags :: Ord a => [[a]] -> [[a]]
diags g = rmdups (upright ++ bottomleft ++ upleft ++ bottomright)
   where
      fitsize xs = winnum <= length xs
      ms = [0..size-1]
      ns = ms
      g' = map reverse g
      upright     = [[g !! n !! (n+m) | n <- ns, n+m < size] | m <- ms]
      bottomleft  = [[g !! (n+m) !! n | n <- ns, n+m < size] | m <- ms]
      upleft      = [[g' !! n !! (n+m) | n <- ns, n+m < size] | m <- ms]
      bottomright = [[g' !! (n+m) !! n | n <- ns, n+m < size] | m <- ms]

tree :: Tree Grid
tree = gametree empty O

nextnode :: Tree Grid -> Grid -> Tree Grid
nextnode (Node _ ts) g = head [Node g' ts' | (Node g' ts') <- ts, g == g']

prepare :: IO ()
prepare = do n <- getTurn
             putStrLn (show n)
             case n of
                1 -> play tree (O,Hum)
                otherwise -> play tree (O,Comp)

play :: Tree Grid -> (Player,Core) -> IO ()
play (Node g ts) (p,w) = do cls
                            goto (1,1)
                            putGrid g
                            play' (Node g ts) (p,w)

play' :: Tree Grid -> (Player,Core) -> IO ()
play' (Node g ts) (p,c)
   | wins O g  = putStrLn "Player O wins!\n"
   | wins X g  = putStrLn "Player X wins!\n"
   | full g    = putStrLn "It's a draw!\n"
   | c == Hum  = do i <- getNat (prompt p)
                    case move g i p of
                       []   -> do putStrLn "ERROR: Invalid move"
                                  play' (Node g ts) (p,c)
                       [g'] -> play (nextnode (Node g ts) g') (next p, nextCore c)

   | c == Comp = do putStrLn ("Player " ++ (show p) ++ " is thinking... ")
                    (play $! (bestnode (Node g ts) p)) (next p, nextCore c)

-- d --------------------

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
   | wins O g   = Node (g, O) []
   | wins X g   = Node (g, X) []
   | otherwise  = Node (g, B) []
minimax (Node g ts) = Node (g, p) (filter (best p) ts')
   where
      ts' = map minimax ts
      ps  = [p | Node (_,p) _ <- ts']
      p   = if turn g == O then minimum ps else maximum ps
      best p (Node (_,p') _) = p' == p

bestnode :: Tree Grid -> Player -> Tree Grid
bestnode tree p = nextnode tree g
               where
                  t = prune depth tree
                  Node (_,_) ts = minimax t
                  g = head [g | Node (g,_) _ <- ts]

