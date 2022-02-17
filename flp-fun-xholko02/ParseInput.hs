module ParseInput
( dispatch
) where

import Data.List
import Simplify

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
    putStrLn (show g2)

firstPart :: [String] -> IO ()
firstPart [fileName] = do
    putStrLn "Chosen option -1"
    putStrLn (show (alg43_1 g2))


completeConvert :: [String] -> IO ()
completeConvert [fileName] = do
    putStrLn "Chosen option -2"
    putStrLn (show (alg43_full g2))