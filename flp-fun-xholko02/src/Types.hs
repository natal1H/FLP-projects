-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Types.hs
-- Popis:   Vlastné dátové typy pre bezkontextovú gramatiku

module Types
( Symbol
, Rule
, BKG (..)
, isBKGValid
) where

import Data.List ( intercalate )

-- Vlastné štruktúry
type Symbol = Char -- (Ne)terminálny symbol
type Rule = (Symbol, [Symbol]) -- pravidlo bezkontextovej gramatiky

--- BKG G = (N, eps, P, S)
data BKG = BKG { nonterminals :: [Symbol] -- zoznam neterminálnych symbolov 
               , terminals    :: [Symbol] -- zoznam terminlálnych symbolov
               , rules        :: [Rule]   -- zoznam pravidiel
               , startSymbol  :: Symbol   -- počiatočný neterminálny symbol
               }

instance Show BKG where
   show (BKG n t p s) =  intercalate "," (map (:[]) n) ++ "\n" ++ -- neterminálne symboly
                         intercalate "," (map (:[]) t) ++ "\n" ++ -- terminálne symbol
                         [s] ++ "\n" ++ -- počiatočný symbol
                         intercalate "\n" (map (\(x, y) -> [x] ++ "->" ++ y) p)

-- Kontrola, či je symbol platný neterminál (A..Z)
isNontermValid :: Symbol -> Bool
isNontermValid n = n `elem` ['A'..'Z']

-- Kontrola, či je symbol platný terminál (a..z) + #
isTermValid :: Symbol -> Bool
isTermValid t = t `elem` '#':['a'..'z']

-- Kontrola, či je pravidlo platné (neterminál -> ((ne)terminál))^+)
isRuleValid :: Rule -> Bool
--isRuleValid r = isNontermValid (fst r) && and [isNontermValid s || isTermValid s | s <- snd r]
isRuleValid r = isNontermValid (fst r) && foldl (\acc x -> acc && (isNontermValid x || isTermValid x)) True (snd r)

-- Kontrola, či je BKG platná 
--isBKGValid :: BKG -> Bool 
isBKGValid :: BKG -> Bool
isBKGValid (BKG n t p s) = 
   let nontermsValid = foldl (\acc x -> acc && isNontermValid x) True n
       termsValid = foldl (\acc x -> acc && isTermValid x) True t
       rulesValid = foldl (\acc x -> acc && isRuleValid x) True p
   in nontermsValid && termsValid && rulesValid && isNontermValid s