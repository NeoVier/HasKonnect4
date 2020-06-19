module Main where

import           CLI.ConnectCLI
import           Data.List          (intercalate)
import           GUI.ConnectGUI
import           System.Environment (getArgs, getProgName)

options :: [([String], String, IO ())]
options =
        [ (["-h", "--help"], "Display this help and exit.", printHelp)
        , (["-v", "--version"], "Display version and exit.", printVersion)
        , (["-c", "--cli"], "Play game on the CLI.", runCLI)
        , (["-g", "--gui"], "Play game on the GUI. (DEFAULT)", runGUI)
        ]

displayFlags :: [String]
displayFlags = map (\(opt, _, _) -> "[" ++ intercalate "|" opt ++ "]") options

displayOptions :: [String]
displayOptions =
        map (\(opt, desc, _) -> intercalate "," opt ++ separator opt ++ desc)
                options
  where
    separator opt =
            replicate
                    (maximum (map length displayFlags) + 3 -
                     length (unwords opt))
                    ' '

printHelp :: IO ()
printHelp = do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " " ++ unwords displayFlags ++ "\n"
        putStrLn "Available options: "
        putStrLn $ unlines displayOptions

printVersion :: IO ()
printVersion = putStrLn "HasKonnect4 - v1.0"

getOptionFunction :: String -> IO ()
getOptionFunction x = do
        let opt = filter (\(opt, _, _) -> x `elem` opt) options
        case opt of
                []          -> defaultCommand
                (_, _, f):_ -> f

defaultCommand :: IO ()
defaultCommand = printHelp

main :: IO ()
main = do
        args <- getArgs
        if null args
                then defaultCommand
                else getOptionFunction (head args)
