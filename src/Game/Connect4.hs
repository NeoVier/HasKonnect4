module Game.Connect4
        ( defaultWidth
        , defaultHeight
        , defaultWinningLength
        , initialGame
        , emptyBoard
        , availableColumns
        , play
        , Game(..)
        , Player(..)
        , Board
        , Column
        ) where

import           Data.List  (group)
import           Data.Maybe (fromJust, isJust, isNothing)
import           Game.Utils

data Player
        = Red
        | Yellow
        deriving (Eq, Enum)

instance Show Player where
        show Red    = "R"
        show Yellow = "Y"

type Board = [[Maybe Player]]

type Column = Int

data Game = Game
        { gBoard  :: Board
        , gPlayer :: Player
        , gWinner :: Maybe Player
        } deriving (Show)

defaultWidth :: Int
defaultWidth = 7

defaultHeight :: Int
defaultHeight = 6

defaultWinningLength :: Int
defaultWinningLength = 4

emptyBoard :: Board
emptyBoard = replicate defaultHeight (replicate defaultWidth Nothing)

initialGame :: Game
initialGame = Game emptyBoard Red Nothing

fallAmt :: Board -> Column -> Int
fallAmt b c = length (takeWhile isNothing (getColumn c b)) - 1

checkWin :: Board -> Maybe Player
checkWin b = firstJusts $ map checkRow allLines
  where
    allLines = b ++ transpose b ++ diagonals b

checkRow :: [Maybe Player] -> Maybe Player
checkRow r
        | null winners = Nothing
        | otherwise = head $ head winners
  where
    winners = filter ((>= defaultWinningLength) . length) (group r)

move :: Player -> Column -> Board -> Board
move p c b
        | fallAmt b c >= 0 = replaceNth2D (fallAmt b c) c (Just p) b
        | otherwise = b

play :: Column -> Game -> Game
play col game
        | isJust (gWinner game) = game
        | newBoard == gBoard game = game
        | otherwise = Game newBoard newPlayer (checkWin newBoard)
  where
    newBoard = move (gPlayer game) col (gBoard game)
    winner = checkWin newBoard
    newPlayer
            | isNothing winner = cycleData (gPlayer game)
            | otherwise = gPlayer game

availableColumns :: Board -> [Column]
availableColumns b =
        [i | i <- [0 .. length b - 1], any isNothing (transpose b !! i)]
