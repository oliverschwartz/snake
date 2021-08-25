module Main where

import System.IO
import System.Exit
import System.Console.ANSI (clearScreen, setCursorPosition, hideCursor)
import System.Random (randomRIO)
import Control.Concurrent
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, NominalDiffTime)
import Data.Maybe (isNothing)
import Snake 

-- Data types. 
data World = World {
    lastUpdateTime  :: UTCTime,
    snake           :: Snake, 
    dir             :: Dir, 
    cycles          :: Int,
    food            :: Maybe (Int, Int)
}

-- Constants. 
boardSize :: Int 
boardSize = 20
foodCycles :: Int
foodCycles = 10
loff :: Int
loff = 2
toff :: Int
toff = 2
moveTime :: NominalDiffTime
moveTime = 0.25

makeWorld :: UTCTime -> World 
makeWorld timePoint = World timePoint newSnake R 0 Nothing

-- Output a character by moving the cursor there.
printChar :: Char -> (Int, Int) -> IO ()
printChar c (row, col) = do
    setCursorPosition row col
    putChar c

printFood :: Maybe (Int, Int) -> Char -> IO ()
printFood Nothing _ = return ()
printFood (Just (x, y)) c = printChar c (boardSize - y + toff, x + loff + 1)

renderWorld :: World -> World -> IO ()
renderWorld world oldWorld = do
    let coors = snakeToCoors (snake world)
        oldCoors = snakeToCoors (snake oldWorld)
        
    -- Game borders.
        left = [(row,loff) | row <- [toff..toff+boardSize+1]]
        right = [(row,loff+boardSize+1) | row <- [toff..toff+boardSize+1]]
        top = [(toff,col) | col <- [loff..loff+boardSize+1]]
        bottom = [(toff+boardSize+1,col) | col <- [loff..loff+boardSize+1]]
    mapM_ (\l -> mapM_ (printChar '+') l) [left, right, top, bottom]

    -- Snake pieces (old then new). 
    let f c (x, y) = printChar c (boardSize - y + toff, x + loff + 1)
    mapM_ (f ' ') oldCoors
    mapM_ (f 's') coors
    printFood (food oldWorld) ' '
    printFood (food world) 'f'
    return ()
    
-- Generate a random coordinate (outside the snake) for food. 
spawnFood :: Snake -> IO (Int, Int)
spawnFood s = do
    let coors = snakeToCoors s
    x <- randomRIO (0, boardSize-1)
    y <- randomRIO (0, boardSize-1)
    let dist = distFromSnake s (x,y)
    if not (elem (x,y) coors) && dist > 3
        then return (x,y) 
        else spawnFood s

eatFood :: Snake -> World -> (Snake, World)
eatFood s world = 
    case food world of 
        Nothing -> (s, world)
        Just (x,y) -> 
            if snakeContains s (x,y)
                then (growSnake s (dir world), world { food = Nothing })
                else (s, world)
    
updateWorld :: World -> UTCTime -> IO World
updateWorld world timePoint = 
    if diffUTCTime timePoint (lastUpdateTime world) > moveTime
        then do 

            -- Move the snake.
            let snake' = moveSnake (snake world) (dir world)
            if not (validateSnake snake' boardSize)
                then gameOver world
                else return () 
            
            -- If we ate food, grow the snake. 
            let (snake'', world') = eatFood snake' world
            
            -- If enough cycles have passed, spawn fruit.
            let cycles' = (cycles world + 1) `mod` foodCycles
            spawnedFood <- spawnFood snake''
            let food' = if isNothing (food world')
                then Just spawnedFood
                else food world

            -- Update the world.
            let world'' = world' { lastUpdateTime = timePoint
                               , snake = snake''
                               , cycles = cycles'
                               , food = food'}
            renderWorld world'' world
            return(world'')
        else return(world)

{- Process input character.
 - Direction cannot go from R to L or U to D (or vice versa). -}
getCurrentDir :: World -> Maybe Char -> Dir
getCurrentDir world Nothing = dir world
getCurrentDir world (Just c)
    | c == 'h' && d /= R = L 
    | c == 'j' && d /= U = D
    | c == 'k' && d /= D = U
    | c == 'l' && d /= L = R
    | otherwise = d
    where d = dir world

-- End the game and print the score.
gameOver :: World -> IO ()
gameOver world = do 
    let len = snakeLen (snake world)
    clearScreen
    setCursorPosition 0 0
    putStrLn "GAME OVER"
    putStr $ "SCORE: " ++ show len ++ "\n"
    exitSuccess

gameLoop :: ThreadId -> MVar Char -> World -> IO ()
gameLoop inputThId input world = do 
    timePoint <- getCurrentTime
    tryInput <- tryTakeMVar input
    let newDir = getCurrentDir world tryInput
    let world' = world { dir = newDir }
    world'' <- updateWorld world' timePoint
    gameLoop inputThId input world''

inputLoop :: MVar Char -> IO ()
inputLoop input = (putMVar input =<< getChar) >> inputLoop input

main :: IO ()
main = do 

    -- IO operations. 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    setCursorPosition 0 0
    hideCursor

    -- Start IO thread and game-loop. 
    input <- newEmptyMVar
    inputThId <- forkIO $ inputLoop input
    timePoint <- getCurrentTime
    gameLoop inputThId input (makeWorld timePoint)
