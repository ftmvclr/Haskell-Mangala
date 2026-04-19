module Main where
import Data.Bits (Bits(xor))

p1Board :: [Int]
p1Board = [4,4,4,4,4,4]
p2Board :: [Int]
p2Board = [4,4,4,4,4,4]
p1Box :: Int
p1Box = 0
p2Box :: Int
p2Box = 0

game :: ([Int], [Int], Int, Int, Int) -> ([Int], [Int], Int ,Int, Int) --p1board p2board p1box p2Box, whoseTurn
game p1board p2board p1box p2Box whoseTurn = do
    putStrLn $"Player {whoseTurn} enter hole number (1-6):"
    input <- getLine 
    let hole = read input :: Int 
    putStrLn ("You selected hole: " ++ show hole)
-- first things first
    if whoseTurn == 1 
        then 
            if peekPlayersBoard (hole, p1board) == 1
                then handle1StoneCase -- TODO function
                else game (tupleAdjuster1 (distStonesP1 p1Board p2Board p1Box (peekPlayersBoard hole p1board) hole whoseTurn) p2Box)
        else 
            if peekPlayersBoard (hole, p2board) == 1
                then handle1StoneCase -- TODO function
                else game (tupleAdjuster2 (distStonesP2 p2Board p1Board p2Box (peekPlayersBoard hole p2board) hole whoseTurn) p1Box)
    
peekPlayersBoard :: (Int, [Int]) -> Int
peekPlayersBoard (hole, board) = board !! (hole + 1)

tupleAdjuster1 :: ([Int], [Int], Int, Int, Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster1 (p1Board, p2Board, p1Box, _, _, whoseTurn) p2Box = (p1Board, p2Board, p1Box, p2Box, whoseTurn)


tupleAdjuster2 :: ([Int], [Int], Int, Int, Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster2 (p2Board, p1Board, p2Box, _, _, whoseTurn) p1Box = (p1Board, p2Board, p1Box, p2Box, whoseTurn)

distStonesP1 :: ([Int], [Int], Int, Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) --p1board p2board p1box, stoneCount, index, whoseTurn
distStonesP1 (p1Board, p2Board, p1Box, stoneCount, index)
    = distribute (p1Board ++ p1Box ++ p2Board, stoneCount, index)

distStonesP2 :: ([Int], [Int], Int, Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) --p2board p1board p2box, stoneCount, index, whoseTurn
distStonesP2 (p2Board, p1Board, p2Box, stoneCount, index)
    = distribute (p2Board ++ p2Box ++ p1Board, stoneCount, index)

distribute :: ([Int], Int, Int, Int) -> ([Int], Int, Int, Int)  -- concatenated list, stoneCount, index, whoseTurn
distribute (list, stoneCount, index)
    | stoneCount == 1 = peekLastStone
    | otherwise = distribute(take index board ++ [(list !! index) + 1] ++ drop (index + 1) board, stoneCount - 1, index + 1)
        
peekLastStone :: ([Int], Int, Int) -> ([Int], Int, Int) -- concat list, index, whoseTurn
peekLastStone 

handle1StoneCase 
main :: IO ()
main = putStrLn "Hello, Haskell :)BbB!"
-- dist stones (player1 and 2 seperately)
    -- peekLastStone, displayBoard
-- a logic to keep turns
-- isGameOver check both players' holes
-- emptyBoard (isgameover calls it in case of a winner)
