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

game :: ([Int], [Int], Int, Int, Int) -> ([Int], [Int], Int ,Int, Int) --p1board p2board p1box p2Box, whose turn
game p1board p2board p1box p2Box whoseTurn = do
    putStrLn $"Player {whoseTurn} enter hole number (1-6):"
    input <- getLine 
    let hole = read input :: Int 
    putStrLn ("You selected hole: " ++ show hole)
-- first things first
    if whoseTurn == 1 
        then 
            if (peekPlayersBoard hole p1board) == 1
                then handleCase1 -- TODO function
                else distStonesP1 p1Board p2Board p1Box (peekPlayersBoard hole p1board) hole
        else 
            if (peekPlayersBoard hole p2board) == 1
                then handleCase1 -- TODO function
                else distStonesP2 p2Board p1Board p2Box (peekPlayersBoard hole p2board) hole

distStonesP1 :: ([Int], [Int], Int, Int, Int) -> ([Int], [Int], Int ,Int, Int) --p1board p2board p1box, stoneCount, index
distStonesP1 p1Board p2Board p1Box x
    |x == 0 = peekLastStone
    |otherwise = distribute 

peekPlayersBoard :: Int -> [Int] -> Int-- index, board
    peekPlayersBoard index board = board !! (index+1)

distribute :: ([Int], Int, Int) -> ([Int], Int, Int)  -- concatenated list, stonecount index
main :: IO ()
main = putStrLn "Hello, Haskell :)BbB!"
-- dist stones (player1 and 2 seperately)
    -- peekLastStone, displayBoard
-- a logic to keep turns
-- isGameOver check both players' holes
-- emptyBoard (isgameover calls it in case of a winner)
