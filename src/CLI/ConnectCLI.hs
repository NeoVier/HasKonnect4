module CLI.ConnectCLI
        ( runCLI
        ) where

import           Data.Char           (isDigit)
import           Data.Maybe          (isNothing)
import           Game.Connect4
import           System.Console.ANSI
import qualified System.Console.ANSI as SCA
import           System.Exit         (exitFailure, exitSuccess)

eraseBoard :: Board -> IO ()
eraseBoard b = do
        cursorUp (length b + 3)
        clearFromCursorToScreenEnd

showBoard :: Board -> String
showBoard = unlines . map (unwords . map (maybe "-" show))

printPlayer :: Maybe Player -> IO ()
printPlayer Nothing =
        setSGR [SetColor Foreground Vivid SCA.White] >> putStr "- "
printPlayer (Just Game.Connect4.Red) =
        setSGR [SetColor Foreground Vivid SCA.Red] >> putStr "R "
printPlayer (Just Game.Connect4.Yellow) =
        setSGR [SetColor Foreground Vivid SCA.Yellow] >> putStr "Y "

printGame :: Game -> IO ()
printGame g = do
        if gBoard g == emptyBoard
                then putStr ""
                else eraseBoard (gBoard g)
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
                "To play, type in the number of the column you want to place your disk in, 'q' to quit or 'r' to restart.\n"

getOption :: [Column] -> IO Column
getOption allowed = do
        putStr "> "
        option <- getLine
        if option == "q"
                then exitSuccess
                else if option == "r"
                             then return (-1)
                             else if not (null option) &&
                                     all isDigit option &&
                                     read option `elem` allowed
                                          then return (read option)
                                          else do
                                                  cursorUp 1
                                                  clearFromCursorToLineEnd
                                                  getOption allowed

step :: Game -> IO Game
step g = do
        printGame g
        option <- getOption [0 .. length (gBoard g)]
        if option == -1
                then do
                        eraseBoard (gBoard g)
                        step initialGame
                else step (play option g)

runCLI :: IO ()
runCLI = do
        printHelp
        step initialGame
        putStrLn "Game Finished"
