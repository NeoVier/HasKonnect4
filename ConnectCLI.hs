module ConnectCLI
        (
        ) where

import           Data.Char           (isDigit)
import           Data.Maybe          (isNothing)
import           Game.Connect4
import           System.Console.ANSI
import           System.Exit         (exitFailure, exitSuccess)

showBoard :: Board -> String
showBoard = unlines . map (unwords . map (maybe "-" show))

printPlayer :: Maybe Player -> IO ()
printPlayer Nothing = setSGR [SetColor Foreground Vivid White] >> putStr "- "
printPlayer (Just Game.Connect4.Red) =
        setSGR [SetColor Foreground Vivid System.Console.ANSI.Red] >>
        putStr "R "
printPlayer (Just Game.Connect4.Yellow) =
        setSGR [SetColor Foreground Vivid System.Console.ANSI.Yellow] >>
        putStr "Y "

printGame :: Game -> IO ()
printGame g = do
        if gBoard g == emptyBoard
                then cursorUp (-1)
                else do
                        cursorUp (length (gBoard g) + 3)
                        clearFromCursorToScreenEnd
        putStr "Player: "
        printPlayer (Just (gPlayer g))
        if isNothing (gWinner g)
                then putStrLn ""
                else putStrLn " (WINNER!)"
        setSGR [SetColor Foreground Vivid White]
        putStrLn $ unwords $ map show [0 .. length (gBoard g)]
        mapM_ (\p -> mapM_ printPlayer p >> putStrLn "") (gBoard g)
        setSGR [SetColor Foreground Vivid White]

printHelp :: IO ()
printHelp = do
        putStrLn "Welcome to Connect4 CLI!"
        putStrLn
                "To play, type in the number of the column you want to place your disk in or 'q' to quit."

getOption :: [Column] -> IO Column
getOption allowed = do
        putStr "> "
        option <- getLine
        if option == "q"
                then exitSuccess
                else if not (null option) &&
                        all isDigit option && read option `elem` allowed
                             then return (read option)
                             else do
                                     cursorUp 1
                                     clearFromCursorToLineEnd
                                     getOption allowed

step :: Game -> IO Game
step g = do
        printGame g
        option <- getOption [0 .. length (gBoard g)]
        step (play option g)

main :: IO ()
main = do
        printHelp
        step initialGame
        putStrLn "Game Finished"
