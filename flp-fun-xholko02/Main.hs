
import System.Environment
import System.Directory
import System.IO
import Data.List

-- Custom structures

type Symbol = Char -- (Non)terminal symbol
type Rule = (Symbol, [Symbol]) -- Context-free grammar rule

-- Cmd line arg parsing

-- dispatch association list - takes arg list as param and returns IO action
dispatch :: [(String, [String] -> IO())]
dispatch = [ ("-i", onlyDisplay) -- TODO: find a better name
           , ("-1", firstPart) -- TODO: find a better name
           , ("-2", completeConvert) -- TODO: find a better name
           ]

onlyDisplay :: [String] -> IO ()
onlyDisplay [fileName] = do
    putStrLn "Chosen option -i"

firstPart :: [String] -> IO ()
firstPart [fileName] = do
    putStrLn "Chosen option -1"

completeConvert :: [String] -> IO ()
completeConvert [fileName] = do
    putStrLn "Chosen option -2"

main = do
    (command:args) <- getArgs -- command should be -i/-1/-2
    let (Just action) = lookup command dispatch -- lookup command in dispatch list
    action args -- will return IO action
