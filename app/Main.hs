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

distStonesP1 :: ([Int], [Int], Int, Int, Int) -> ([Int], [Int], Int ,Int, Int) --p1board p2board p1box, stoneCount, index
distStonesP1 p1Board p2Board p1Box x
    |x == 0 = peekLastStone 
    |p1Board !! index == 1

main :: IO ()
main = do 
    putStrLn "Hello, Haskell :)BbB!"
    putStrLn "Hello, Haskell :)BbB!"
-- dist stones (player1 and 2 seperately)
    -- peekLastStone, displayBoard
-- a logic to keep turns
-- isGameOver check both players' holes
-- emptyBoard (isgameover calls it in case of a winner)
