-- Snake.hs

module Snake where

data Dir = U | D | L | R deriving (Eq, Show)
data Piece = Piece Int Int deriving (Eq, Show)
data Snake = Snake [Piece] deriving (Show)
data Food = Food Int Int deriving (Eq, Show)

newSnake :: Snake
newSnake = Snake [Piece 0 0]

snakeContains :: Snake -> (Int, Int) -> Bool
snakeContains (Snake pcs) (x,y) = elem (Piece x y) pcs

-- Check the snake is valid (no overlaps and inside borders).
validateSnake :: Snake -> Int -> Bool
validateSnake s@(Snake pcs) boardSize = snakeCollision s && 
                                        snakeInBorders s boardSize

snakeToCoors :: Snake -> [(Int, Int)]
snakeToCoors (Snake []) = []
snakeToCoors (Snake ((Piece x y):pcs)) = (x,y) : snakeToCoors (Snake pcs)
    
snakeLen :: Snake -> Int
snakeLen (Snake pcs) = length pcs

-- Check the snake is inside the borders.
snakeInBorders :: Snake -> Int -> Bool
snakeInBorders (Snake pcs) boardSize = all f pcs where 
    range = [0..boardSize-1]
    f (Piece x y) = elem x range && elem y range

-- Check the snake isn't eating itself.
snakeCollision :: Snake -> Bool
snakeCollision (Snake pcs) = go pcs [] where
    go lst acc = 
        case lst of 
            [] -> True
            hd:tl -> if elem hd acc then False else go tl (hd:acc)

-- Move the snake along in the given direction. 
moveSnake :: Snake -> Dir -> Snake
moveSnake s@(Snake []) _ = s
moveSnake (Snake (p:[])) dir = Snake [movePiece p dir]
moveSnake (Snake (p:pcs)) dir = 
    let newHead = movePiece p dir in 
    let excludeLast = take (length pcs - 1) pcs in 
    Snake (newHead : p : excludeLast)

-- Grow the snake. 
growSnake :: Snake -> Dir -> Snake 
growSnake (Snake []) _ = Snake []
growSnake (Snake (hd@(Piece x y):[])) dir = 
    let tl = case dir of 
            U -> Piece x (y-1)
            D -> Piece x (y+1)
            L -> Piece (x+1) y
            R -> Piece (x-1) y
    in Snake [hd, tl]
growSnake (Snake pcs) _ = 
    let len = length pcs
        (Piece x y, Piece x' y') = (pcs !! (len-1), pcs !! (len-2))
        xDiff = x - x' 
        yDiff = y - y'
    in Snake (pcs ++ [Piece (x + xDiff) (y + yDiff)])


-- Move a piece along its direction if none provided. 
movePiece :: Piece -> Dir -> Piece 
movePiece (Piece x y) dir = case dir of
    U -> Piece x (y+1)
    D -> Piece x (y-1)
    L -> Piece (x-1) y
    R -> Piece (x+1) y

-- The smallest Manhattan distance from any piece of the snake.
distFromSnake :: Snake -> (Int, Int) -> Int
distFromSnake (Snake []) _ = maxBound :: Int
distFromSnake (Snake ((Piece x y):pcs)) t@(x',y') = 
    let dist = abs (x - x') + abs (y - y') in 
    min dist (distFromSnake (Snake pcs) t)
