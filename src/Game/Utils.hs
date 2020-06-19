module Game.Utils where

import           Data.Maybe (isJust)

getColumn :: Int -> [[a]] -> [a]
getColumn c = map (!! c)

transpose :: [[a]] -> [[a]]
transpose m = map (`getColumn` m) [0 .. length (head m) - 1]

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

getMainDiagonals :: Int -> Int -> [[(Int, Int)]]
getMainDiagonals height width =
        init $
        filter (not . null) $
        map (filter (\(line, col) -> line < height && col < width)) $
        bottomHalf ++ map (map (\(f, s) -> (s, f))) bottomHalf
  where
    order = max height width
    bottomHalf =
            map (\x -> map (\(f, s) -> (f + order - x, s)) $ mainDiagonal x x)
                    [1 .. order]

secondaryDiagonal :: Int -> Int -> [(Int, Int)]
secondaryDiagonal width height =
        zip [height - 1,height - 2 .. 0] [0 .. width - 1]

getSecondaryDiagonals :: Int -> Int -> [[(Int, Int)]]
getSecondaryDiagonals height width =
        init $
        filter (not . null) $
        map (filter (\(line, col) -> line < height && col < width)) $
        topHalf ++ map (map (\(f, s) -> (order - s - 1, order - f - 1))) topHalf
  where
    order = max height width
    topHalf = map (\x -> secondaryDiagonal x x) [1 .. order]

leftToRightDiagonals :: [[a]] -> [[a]]
leftToRightDiagonals m =
        map (map (`get2D` m)) $ getMainDiagonals (length m) (length $ head m)

rightToLeftDiagonals :: [[a]] -> [[a]]
rightToLeftDiagonals m =
        map (map (`get2D` m)) $
        getSecondaryDiagonals (length m) (length $ head m)

diagonals :: [[a]] -> [[a]]
diagonals m = leftToRightDiagonals m ++ rightToLeftDiagonals m

cycleData :: Enum a => a -> a
cycleData cur = allValues !! ((fromEnum cur + 1) `mod` length allValues)
  where
    allValues = [toEnum 0 ..]
