module ParseInput
( dispatch
, readFromStdinBKG
) where

import System.IO
import Data.List
import Data.List.Split
import Simplify
import Types

-- Reading grammar from file
readFromFileBKG :: String -> IO BKG
readFromFileBKG filename = do
    contents <- readFile filename
    let allLines  = lines contents
        nonterms  = concat $ splitOn "," (allLines !! 0)
        terms     = concat $ splitOn "," (allLines !! 1)
        startSym  = head (allLines !! 2)
        rulesStrs = take (length allLines - 3) $ drop 3 allLines -- list of rules in string form
        rulesTpls = map (\x -> let splited = splitOn "->" x
                                   leftSide  = head (splited !! 0)
                                   rightSide = splited !! 1
                               in (leftSide,rightSide)) rulesStrs
    let grammar = BKG { nonterminals=nonterms
                      , terminals=terms
                      , rules=rulesTpls
                      , startSymbol=startSym
                      }
    return grammar

-- To keep correct order
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- Reading grammar from stdin
readFromStdinBKG :: IO ()
readFromStdinBKG = do
    nonterminalsLine <- prompt "Nonterminal symbols (e.g. S,A,B): "
    terminalsLine <- prompt "Terminal symbols (e.g. a,b): "
    startSymbolLine <- prompt "Starting symbol (e.g. S): "
    let nonterms = concat $ splitOn "," nonterminalsLine
        terms    = concat $ splitOn "," terminalsLine
        startSym = head startSymbolLine
    putStrLn nonterms
    putStrLn terms
    print startSym

-- Cmd line arg parsing
-- dispatch association list - takes arg list as param and returns IO action
dispatch :: [(String, [String] -> IO())]
dispatch = [ ("-i", onlyDisplay) -- TODO: find a better name
           , ("-1", firstPart) -- TODO: find a better name
           , ("-2", completeConvert) -- TODO: find a better name
           ]

onlyDisplay :: [String] -> IO ()
onlyDisplay [] = do
    putStrLn "Chosen option -i from stdin"
    -- TODO: read from stdin
onlyDisplay (fileName:_) = do
    putStrLn "Chosen option -i from file"
    grammar <- readFromFileBKG fileName
    print grammar

firstPart :: [String] -> IO ()
firstPart [] = do
    putStrLn "Chosen option -1 from stdin"
    -- TODO: read from stdin
firstPart (fileName:_) = do
    putStrLn "Chosen option -1 from file"
    grammar <- readFromFileBKG fileName
    print (alg43_1 grammar)


completeConvert :: [String] -> IO ()
completeConvert [] = do
    putStrLn "Chosen option -2 from stdin"
    -- TODO: read from stdin
completeConvert (fileName:_) = do
    putStrLn "Chosen option -2 from file"
    grammar <- readFromFileBKG fileName
    print (alg43_full grammar)