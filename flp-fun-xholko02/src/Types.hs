module Types
( Symbol
, Rule
, BKG (..)
) where

import Data.List
-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Types.hs
-- Popis:   Vlastné dátové typy pre bezkontextovú gramatiku

-- Custom structures
type Symbol = Char -- (Non)terminal symbol
type Rule = (Symbol, [Symbol]) -- Context-free grammar rule

--- BKG G = (N, eps, P, S)
data BKG = BKG { nonterminals :: [Symbol] -- list of nonterminal
               , terminals    :: [Symbol] -- list of termimal symbols
               , rules        :: [Rule]   -- list of rules
               , startSymbol  :: Symbol   -- starting nonterminal symbol
               }

instance Show BKG where
   show (BKG n t p s) =  intercalate "," (map (:[]) n) ++ "\n" ++ -- neterminálne symboly
                         intercalate "," (map (:[]) t) ++ "\n" ++ -- terminálne symbol
                         [s] ++ "\n" ++ -- počiatočný symbol
                         intercalate "\n" (map (\(x, y) -> [x] ++ "->" ++ y) p)