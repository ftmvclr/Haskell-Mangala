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
game p1Board p2Board p1Box p2Box whoseTurn = do
    putStrLn $"Player {whoseTurn} enter hole number (1-6):"
    input <- getLine 
    let hole = read input :: Int 
    putStrLn ("You selected hole: " ++ show hole)
-- first things first
    if whoseTurn == 1 
        then 
            if peekPlayersBoard (index, p1board) == 1
                then game(tupleAdjuster1'(handle1StoneCase1 p1Board ++ p1Box ++ p2Board index whoseTurn) hole whoseTurn )
                else game (tupleAdjuster1 (distStonesP1 p1Board p2Board p1Box (peekPlayersBoard index p1board) hole whoseTurn) p2Box)
        else 
            if peekPlayersBoard (index, p2board) == 1
                then game(tupleAdjuster2'(handle1StoneCase2 (p2Board p1Board p2Box index) index whoseTurn p1Box))
                else game (tupleAdjuster2 (distStonesP2 p2Board p1Board p2Box (peekPlayersBoard index p2board) hole whoseTurn) p1Box)
    where 
        index = hole - 1

tupleAdjuster1' :: ([Int], Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster1' (concatList, _, whoseTurn) p2Box = (take 6 concatList, drop 7 concatList, concatList !! 6, p2Box, whoseTurn)

tupleAdjuster2' :: ([Int], Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster2' (concatList, _, whoseTurn) p1Box = ( drop 7 concatList, take 6 concatList, p1Box, concatList !! 6, whoseTurn)

handle1StoneCase1 :: ([Int], [Int], Int, Int) -> ([Int], Int, Int)
handle1StoneCase1 (p1Board, p2Board, p1Box, index) = peekLastStone (p1Board ++ [p1Box] ++ p2Board, index, 1)

handle1StoneCase2 :: ([Int], Int, Int) -> ([Int], Int, Int)
handle1StoneCase2 =  peekLastStone

peekPlayersBoard :: (Int, [Int]) -> Int
peekPlayersBoard (index, board) = board !! index

tupleAdjuster1 :: ([Int], [Int], Int, Int, Int, Int) -> Int -> ([Int], [Int], Int, Int, Int) -- stonecounter index
tupleAdjuster1 (p1Board, p2Board, p1Box, _, _, whoseTurn) p2Box = (p1Board, p2Board, p1Box, p2Box, whoseTurn)

tupleAdjuster2 :: ([Int], [Int], Int, Int, Int, Int) -> Int -> ([Int], [Int], Int, Int, Int)
tupleAdjuster2 (p2Board, p1Board, p2Box, _, _, whoseTurn) p1Box = (p1Board, p2Board, p1Box, p2Box, whoseTurn)

distStonesP1 :: ([Int], [Int], Int, Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) --p1board p2board p1box, stoneCount, index, whoseTurn
distStonesP1 (p1Board, p2Board, p1Box, stoneCount, index)
    = distribute ((take index p1Board ++ [1] ++ drop (index + 1) p1Board) ++ p1Box ++ p2Board, stoneCount, index)

distStonesP2 :: ([Int], [Int], Int, Int, Int, Int) -> ([Int], [Int], Int ,Int, Int, Int) --p2board p1board p2box, stoneCount, index, whoseTurn
distStonesP2 (p2Board, p1Board, p2Box, stoneCount, index)
    = distribute ((take index p2Board ++ [1] ++ drop (index + 1) p2Board) ++ p2Box ++ p1Board, stoneCount, index)

distribute :: ([Int], Int, Int, Int) -> ([Int], Int, Int, Int)  -- concatenated list, stoneCount, index, whoseTurn
distribute (list, stoneCount, index)
    | stoneCount == 1 = peekLastStone
    | otherwise = distribute(take index list ++ [(list !! index) + 1] ++ drop (index + 1) list, stoneCount - 1, index + 1)
        
peekLastStone :: ([Int], Int, Int) -> ([Int], Int, Int) -- concat list, index, whoseTurn
peekLastStone (list, index, whoseTurn) 
    | index < 6 = if list !! index == 0 -- our last piece came to an empty hole
        then if b  == 0 -- opposing hole is also empty
            then (take index list ++ [1] ++ drop (index + 1) list, index, d) 
            else (take 6 list ++ [a + b] ++ take (5 - index) c ++ [0] ++ drop (6 - index) c, index, d) -- opposing hole isn't empty
        else (take index list ++ [a + 1] ++ drop (index + 1) list, index, d) -- OUR hole wasn't empty anyway
    | index == 6 = (take 6 list ++ [a + b] ++ c, index, e)
    | otherwise = (take index list ++ [f + 1] ++ drop (index + 1) list, index, d) 

    where 
        a = list !! 6 -- current box #stones
        b = list !! (12 - index) -- opposing box #stones
        c :: [Int]
        c = drop 7 list
        d = if whoseTurn == 1 then 2 else 1 
        e = if whoseTurn == 1 then 1 else 2 
        f = list !! index

main :: IO ()
main = putStrLn "Hello, Haskell :)BbB!"
-- dist stones (player1 and 2 seperately)
    -- peekLastStone, displayBoard
-- a logic to keep turns
-- isGameOver check both players' holes
-- emptyBoard (isgameover calls it in case of a winner)
