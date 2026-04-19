module Main where
import Data.List (intercalate) 

p1Board :: [Int]
p1Board = [4,4,4,4,4,4]
p2Board :: [Int]
p2Board = [4,4,4,4,4,4]
p1Box :: Int
p1Box = 0
p2Box :: Int
p2Box = 0

game :: ([Int], [Int], Int, Int, Int) -> IO ([Int], [Int], Int ,Int, Int) --p1board p2board p1box p2Box, whoseTurn
game (p1B, p2B, p1bx, p2bx, whoseTurn) = do
    if isGameOver p1B p2B
        then winningMove p1B p2B p1bx p2bx
        else do
            putStrLn "\n--- CURRENT BOARD ---"
            printBoard p1B p2B p1bx p2bx
            
            putStrLn ("Player " ++ show whoseTurn ++ " enter hole number (1-6):")
            input <- getLine 
            let hole = read input :: Int 
            putStrLn ("You selected hole: " ++ show hole)
            let index = hole - 1            
            if whoseTurn == 1 
                then 
                    if peekPlayersBoard (index, p1B) == 1
                        then game (tupleAdjuster1' (handle1StoneCase1 (p1B, p2B, p1bx, index)) p2bx)
                        else game (tupleAdjuster1 (distStonesP1 (p1B, p2B, p1bx, peekPlayersBoard (index, p1B), index, whoseTurn)) p2bx)
                else 
                    if peekPlayersBoard (index, p2B) == 1
                        then game (tupleAdjuster2' (handle1StoneCase2 (p2B, p1B, p2bx, index)) p1bx)
                        else game (tupleAdjuster2 (distStonesP2 (p2B, p1B, p2bx, peekPlayersBoard (index, p2B), index, whoseTurn)) p1bx)
isGameOver :: [Int] -> [Int] -> Bool
isGameOver p1B p2B = sum p1B == 0 || sum p2B == 0

winningMove :: [Int] -> [Int] -> Int -> Int -> IO ([Int], [Int], Int, Int, Int) 
winningMove p1B p2B p1bx p2bx 
    | score1 == score2 = do
        putStrLn $ "It's a draw! with the score: " ++ show score1 
        -- Hand back the final state so the compiler is happy
        return (p1B, p2B, score1, score2, 0) 
    | score2 > score1  = do
        putStrLn $ "Player 2 won, congratulations with the score: " ++ show score2 ++ " : " ++ show score1 
        return (p1B, p2B, score1, score2, 0)
    | otherwise        = do
        putStrLn $ "Player 1 won, congratulations with the score: " ++ show score1 ++ " : " ++ show score2 
        return (p1B, p2B, score1, score2, 0)
    where 
        p1Holes = sum p1B
        p2Holes = sum p2B
        score1 = if p1Holes == 0 then p1bx + p2Holes else p1bx
        score2 = if p2Holes == 0 then p2bx + p1Holes else p2bx

tupleAdjuster1' :: ([Int], Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster1' (concatList, index, whoseTurn) p2bx = (take 6 concatList, drop 7 concatList, concatList !! 6, p2bx, whoseTurn)

tupleAdjuster2' :: ([Int], Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster2' (concatList, index, whoseTurn) p1bx = (drop 7 concatList, take 6 concatList, p1bx, concatList !! 6, whoseTurn)

handle1StoneCase1 :: ([Int], [Int], Int, Int) -> ([Int], Int, Int)
handle1StoneCase1 (p1B, p2B, p1bx, index) = peekLastStone (take index p1B ++ [0] ++ drop (index + 1) p1B ++ [p1bx] ++ p2B, index + 1, 1)

handle1StoneCase2 :: ([Int], [Int], Int, Int) -> ([Int], Int, Int) --concatlist index whose
handle1StoneCase2 (p2B, p1B, p2bx, index) = peekLastStone (take index p2B ++ [0] ++ drop (index + 1) p2B ++ [p2bx] ++ p1B, index + 1, 2)

peekPlayersBoard :: (Int, [Int]) -> Int
peekPlayersBoard (index, board) = board !! index

tupleAdjuster1 :: ([Int], [Int], Int, Int, Int, Int) -> Int -> ([Int], [Int], Int, Int, Int) -- stonecounter index
tupleAdjuster1 (p1B, p2B, p1bx, _, _, whoseTurn) p2bx = (p1B, p2B, p1bx, p2bx, whoseTurn)

tupleAdjuster2 :: ([Int], [Int], Int, Int, Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster2 (p2B, p1B, p2bx, _, _, whoseTurn) p1bx = (p1B, p2B, p1bx, p2bx, whoseTurn)

distStonesP1 :: ([Int], [Int], Int, Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) --p1board p2board p1box, stoneCount, index, whoseTurn
distStonesP1 (p1B, p2B, p1bx, stoneCount, index, whoseTurn)
    = tupleAdjuster'' (distribute (take index p1B ++ [1] ++ drop (index + 1) p1B ++ [p1bx] ++ p2B, index + 1, 1, stoneCount - 1))

distStonesP2 :: ([Int], [Int], Int, Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) --p2board p1board p2box, stoneCount, index, whoseTurn
distStonesP2 (p2B, p1B, p2bx, stoneCount, index, whoseTurn)
    = tupleAdjuster'' (distribute (take index p2B ++ [1] ++ drop (index + 1) p2B ++ [p2bx] ++ p1B, index + 1, 2, stoneCount - 1))

tupleAdjuster'' :: ([Int], Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) -- distStones1 -
tupleAdjuster'' (concatList, index, whoseTurn, stoneCount) = (take 6 concatList, drop 7 concatList, concatList !! 6, stoneCount, index, whoseTurn)

distribute :: ([Int], Int, Int, Int) -> ([Int], Int, Int, Int)  -- concatenated list, index, whoseTurn, stoneCount
distribute (list, index, whoseTurn, stoneCount)
    | stoneCount == 1 = (first (peekLastStone (list, index, whoseTurn)), second (peekLastStone (list, index, whoseTurn)), third (peekLastStone (list, index, whoseTurn)), stoneCount) -- concat list, index, whoseTurn
    | otherwise = distribute (take index list ++ [(list !! index) + 1] ++ drop (index + 1) list, (index + 1 ) `mod` 13, whoseTurn, stoneCount - 1)
        
peekLastStone :: ([Int], Int, Int) -> ([Int], Int, Int) -- concat list, index, whoseTurn
peekLastStone (list, index, whoseTurn) 
    | index < 6 = if list !! index == 0 -- our last piece came to an empty hole
        then if b  == 0 -- opposing hole is also empty
            then (take index list ++ [1] ++ drop (index + 1) list, index, d) 
            else (take 6 list ++ [a + b + 1] ++ take (5 - index) c ++ [0] ++ drop (6 - index) c, index, d) -- opposing hole isn't empty
        else (take index list ++ [f + 1] ++ drop (index + 1) list, index, d) -- OUR hole wasn't empty anyway
    | index == 6 = (take 6 list ++ [a + 1] ++ c, index, e)
    | otherwise = (take index list ++ [f + 1] ++ drop (index + 1) list, index, d) 
    where 
        a = list !! 6 -- current box #stones
        b = list !! (12 - index) -- opposing box #stones
        c :: [Int]
        c = drop 7 list
        d = if whoseTurn == 1 then 2 else 1 
        e = if whoseTurn == 1 then 1 else 2 
        f = list !! index

first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  

printBoard :: [Int] -> [Int] -> Int -> Int -> IO ()
printBoard p1B p2B p1bx p2bx = do
    putStrLn ("P2 Box: " ++ show p2bx)
    putStrLn (intercalate " | " (map show (reverse p2B)))
    putStrLn (intercalate " | " (map show p1B))
    putStrLn ("P1 Box: " ++ show p1bx ++ "\n")

main :: IO ()
main = do
    putStrLn "Which player starts the game?"
    input <- getLine 
    let playerInitial = read input :: Int 
    
    _ <- game (p1Board, p2Board, p1Box, p2Box, playerInitial)
-- isGameOver check both players' holes
-- emptyBoard (isgameover calls it in case of a winner)
    return ()