-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Types.hs
-- Popis:   Vlastné dátové typy pre bezkontextovú gramatiku

module Types
( Symbol
, Rule
, BKG (..)
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
