-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   ParseInput.hs
-- Popis:   Načítanie argumentov z príkazového riadku a bezkontextovej gramatiky

module ParseInput
( dispatch
, readFromStdinBKG
) where

import Data.List.Split ( splitOn )
import Simplify ( algorithm43Partial, algorithm43Full )
import Types ( BKG(..) )

-- Načíta BKG zo súboru
readFromFileBKG :: String -> IO BKG
readFromFileBKG filename = do
    contents <- readFile filename
    let allLines  = lines contents
        nonterms  = concat $ splitOn "," (allLines !! 0)
        terms     = concat $ splitOn "," (allLines !! 1)
        startSym  = head (allLines !! 2)
        rulesStrs = take (length allLines - 3) $ drop 3 allLines -- zoznam pravidiel vo forme reťazcov
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

-- Načíta BKG zo stdin, požaduje rovnaký formát ako v súbore
readFromStdinBKG :: IO BKG
readFromStdinBKG = do
    contents <- getContents
    let allLines  = lines contents
        nonterms  = concat $ splitOn "," (allLines !! 0)
        terms     = concat $ splitOn "," (allLines !! 1)
        startSym  = head (allLines !! 2)
        rulesStrs = take (length allLines - 3) $ drop 3 allLines -- zoznam pravidiel vo forme reťazcov
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
printGrammarUnchanged (fileName:_) = readFromFileBKG fileName >>= print

-- Načíta BKG, vnútorne ju spracuje a vypíše BKG bez neterminálov, ktoré negenerujú terminály (1. časť algoritmu)
printGrammarPartiallyConverted :: [String] -> IO ()
printGrammarPartiallyConverted [] = readFromStdinBKG >>= print . algorithm43Partial
printGrammarPartiallyConverted (fileName:_) = readFromFileBKG fileName >>= print . algorithm43Partial

-- Načíta BKG, vnútorne ju spracuje a vypíše BKG bez zbytočných symbolov (kompletný algoritmus)
printGrammarCompletelyConverted :: [String] -> IO ()
printGrammarCompletelyConverted [] = readFromStdinBKG >>= print . algorithm43Full
printGrammarCompletelyConverted (fileName:_) = readFromFileBKG fileName >>= print . algorithm43Full
