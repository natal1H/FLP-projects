-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   ParseInput.hs
-- Popis:   Načítanie argumentov z príkazového riadku a bezkontextovej gramatiky

module ParseInput
( dispatch
, readFromStdinBKG
, validCommands
, validArguments
) where

import Data.List.Split ( splitOn )
import Simplify ( algorithm43Partial, algorithm43Full )
import Types ( BKG(..), isBKGValid )
import System.Directory ( doesFileExist )

-- Povolené prepínače
validCommands :: [String]
validCommands = ["-i", "-1", "-2"]

-- Funkcia, ktorá kontroluje či bol zadaný správny počet argumentov (1 alebo 2),
-- či je prvý argument medzi povolenými prepínačmi
-- (Nekontroluje existenciu súboru)
validArguments :: [String] -> Bool
validArguments [cmd] = cmd `elem` validCommands
validArguments [cmd, _] = cmd `elem` validCommands
validArguments _ = False

-- Funkcia, ktorá konvertuje reťazec do vnútornej reprezentácie BKG
parseBKGFromString :: String -> BKG
parseBKGFromString contents =
    let allLines  = lines contents
        nonterms  = concat $ splitOn "," (head allLines)
        terms     = concat $ splitOn "," (allLines !! 1)
        startSym  = head (allLines !! 2)
        rulesStrs = take (length allLines - 3) $ drop 3 allLines -- zoznam pravidiel vo forme reťazcov
        rulesTpls = map (\x -> let splited = splitOn "->" x
                                   leftSide  = head (head splited)
                                   rightSide = splited !! 1
                               in (leftSide,rightSide)) rulesStrs
        grammar = BKG { nonterminals=nonterms
                      , terminals=terms
                      , rules=rulesTpls
                      , startSymbol=startSym
                      }
    in grammar

-- Načíta BKG zo súboru
readFromFileBKG :: String -> IO BKG
readFromFileBKG filename = do
    contents <- readFile filename
    let grammar = parseBKGFromString contents
    if isBKGValid grammar
        then return grammar
        else error "Invalid grammar!"

-- Načíta BKG zo stdin, požaduje rovnaký formát ako v súbore
readFromStdinBKG :: IO BKG
readFromStdinBKG = do
    contents <- getContents
    let grammar = parseBKGFromString contents
    if isBKGValid grammar
        then return grammar
        else error "Invalid grammar!"

-- Spracovanie argumentov z príkazového riadku
-- dispatch association zoznam - berie zoznam argumentov ako parameter a vracia IO action
dispatch :: [(String, [String] -> IO())]
dispatch = [ ("-i", printGrammarUnchanged)
           , ("-1", printGrammarPartiallyConverted)
           , ("-2", printGrammarCompletelyConverted)
           ]

-- Načíta BKG, vnútorne ju spracuje a vypíše na stdout nezmenenú
printGrammarUnchanged :: [String] -> IO ()
printGrammarUnchanged [] = readFromStdinBKG >>= print
printGrammarUnchanged (fileName:_) = do
    fileExists <- doesFileExist fileName  -- Kontrola či existuje súbor
    if fileExists
        then readFromFileBKG fileName >>= print
        else error $ "File " ++ fileName ++ " does not exist!"

-- Načíta BKG, vnútorne ju spracuje a vypíše BKG bez neterminálov, ktoré negenerujú terminály (1. časť algoritmu)
printGrammarPartiallyConverted :: [String] -> IO ()
printGrammarPartiallyConverted [] = readFromStdinBKG >>= print . algorithm43Partial
printGrammarPartiallyConverted (fileName:_) = do
    fileExists <- doesFileExist fileName  -- Kontrola či existuje súbor
    if fileExists
        then readFromFileBKG fileName >>= print . algorithm43Partial
        else error $ "File " ++ fileName ++ " does not exist!"

-- Načíta BKG, vnútorne ju spracuje a vypíše BKG bez zbytočných symbolov (kompletný algoritmus)
printGrammarCompletelyConverted :: [String] -> IO ()
printGrammarCompletelyConverted [] = readFromStdinBKG >>= print . algorithm43Full
printGrammarCompletelyConverted (fileName:_) = do
    fileExists <- doesFileExist fileName  -- Kontrola či existuje súbor
    if fileExists
        then readFromFileBKG fileName >>= print . algorithm43Full
        else error $ "File " ++ fileName ++ " does not exist!"
