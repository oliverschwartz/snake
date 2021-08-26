-- Snake.hs

module Snake where

import qualified Data.List.NonEmpty as NE

data Dir = U | D | L | R deriving (Eq, Show)
data Piece = Piece Int Int deriving (Eq, Show)
data Snake = Snake (NE.NonEmpty Piece) deriving (Show)
data Food = Food Int Int deriving (Eq, Show)

newSnake :: Snake
newSnake = Snake (Piece 0 0 NE.:| [])

snakeContains :: Snake -> (Int, Int) -> Bool
snakeContains (Snake pcs) (x,y) = elem (Piece x y) pcs

-- Check the snake is valid (no overlaps and inside borders).
validateSnake :: Snake -> Int -> Bool
validateSnake s boardSize = snakeCollision s && snakeInBorders s boardSize

snakeToCoors :: Snake -> [(Int, Int)]
snakeToCoors (Snake pcs) = NE.toList $ NE.map f pcs 
    where f (Piece x y) = (x, y)
    
snakeLen :: Snake -> Int
snakeLen (Snake pcs) = NE.length pcs

-- Check the snake is inside the borders.
snakeInBorders :: Snake -> Int -> Bool
snakeInBorders (Snake pcs) boardSize = all f pcs where 
    range = [0..boardSize-1]
    f (Piece x y) = elem x range && elem y range

-- Check the snake isn't eating itself.
snakeCollision :: Snake -> Bool
snakeCollision (Snake pcs) = go (NE.toList pcs) [] where
    go lst acc = 
        case lst of 
            [] -> True
            hd:tl -> if elem hd acc then False else go tl (hd:acc)

-- Move the snake along in the given direction. 
moveSnake :: Snake -> Dir -> Snake
moveSnake (Snake (p NE.:| [])) dir = Snake (movePiece p dir NE.:| [])
moveSnake (Snake (p NE.:| pcs)) dir = 
    let newHead = movePiece p dir 
        excludeLast = take (length pcs - 1) pcs
        newTail = p : excludeLast
    in Snake (newHead NE.:| newTail)

-- Grow the snake. 
growSnake :: Snake -> Dir -> Snake 
growSnake (Snake (hd@(Piece x y) NE.:| [])) dir = 
    let tl = case dir of 
            U -> Piece x (y-1)
            D -> Piece x (y+1)
            L -> Piece (x+1) y
            R -> Piece (x-1) y
    in Snake (NE.fromList [hd, tl])
growSnake (Snake pcs) _ = 
    let len = NE.length pcs
        (Piece x y, Piece x' y') = (pcs NE.!! (len-1), pcs NE.!! (len-2))
        xDiff = x - x' 
        yDiff = y - y'
    in Snake $ NE.fromList (NE.toList pcs ++ [Piece (x + xDiff) (y + yDiff)])


-- Move a piece along its direction if none provided. 
movePiece :: Piece -> Dir -> Piece 
movePiece (Piece x y) dir = case dir of
    U -> Piece x (y+1)
    D -> Piece x (y-1)
    L -> Piece (x-1) y
    R -> Piece (x+1) y

-- The smallest Manhattan distance from any piece of the snake.
distFromSnake :: Snake -> (Int, Int) -> Int
distFromSnake (Snake pcs) (x,y) = 
    foldr f (maxBound :: Int) pcs 
    where 
        dist x' y' = abs (x - x') + abs (y - y')
        f (Piece x' y') d = min (dist x' y') d
    
