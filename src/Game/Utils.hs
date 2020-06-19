module Game.Utils where

import           Data.Maybe (isJust)

getColumn :: Int -> [[a]] -> [a]
getColumn c = map (!! c)

transpose :: [[a]] -> [[a]]
transpose m = map (`getColumn` m) [0 .. length m]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n new list
        | n > length list = error "Connect4.replaceNth: Index out of range"
        | otherwise = start ++ (new : tail end)
  where
    (start, end) = splitAt n list

replaceNth2D :: Int -> Int -> a -> [[a]] -> [[a]]
replaceNth2D line col new m = replaceNth line (replaceNth col new (m !! line)) m

get2D :: (Int, Int) -> [[a]] -> a
get2D (line, col) m = (m !! line) !! col

firstJusts :: [Maybe a] -> Maybe a
firstJusts xs
        | null justs = Nothing
        | otherwise = head justs
  where
    justs = filter isJust xs

mainDiagonal :: Int -> Int -> [(Int, Int)]
mainDiagonal width height = zip [0 .. height - 1] [0 .. width - 1]

secondaryDiagonal :: Int -> Int -> [(Int, Int)]
secondaryDiagonal width height = zip [height - 1, height - 2 .. 0] [0 .. width - 1]

directionalDiagonals :: [(Int, Int)] -> [[(Int, Int)]]
directionalDiagonals originalDiagonal =
        map (filter (\(x, y) -> x >= 0 && x < size && y >= 0 && y < size)) $
        originalDiagonal :
        concatMap
                (\shift ->
                         [ map (\(x, y) -> (x - shift, y)) originalDiagonal
                         , map (\(x, y) -> (x + shift, y)) originalDiagonal
                         ])
                [1 .. size `div` 2]
  where
    size = length originalDiagonal

leftToRightDiagonals :: [[a]] -> [[a]]
leftToRightDiagonals m =
        map (map (`get2D` m)) $
        directionalDiagonals (mainDiagonal (length m) (length $ head m))

rightToLeftDiagonals :: [[a]] -> [[a]]
rightToLeftDiagonals m =
        map (map (`get2D` m)) $
        directionalDiagonals (secondaryDiagonal (length m) (length $ head m))

diagonals :: [[a]] -> [[a]]
diagonals m = leftToRightDiagonals m ++ rightToLeftDiagonals m

cycleData :: Enum a => a -> a
cycleData cur = allValues !! ((fromEnum cur + 1) `mod` length allValues)
  where
    allValues = [toEnum 0 ..]
