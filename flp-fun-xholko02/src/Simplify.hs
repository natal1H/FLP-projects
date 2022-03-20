-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Simplify.hs
-- Popis:   Algoritmus na odstránenie zbytočných symbolov z bezkontextovej gramatiky

module Simplify
( algorithm43Partial
, algorithm43Full
) where

import Types ( BKG(..), Rule, Symbol )


-- Funkcia pre Alg 4.1 - pre pravidlo A->alpha kontrola či alpha je v (Ni-1 zjednotenie Sigma)*
isRuleInUnion :: Rule -> [Symbol] -> [Symbol] -> Bool
isRuleInUnion rule_i nonterm_i sigma =
    let union = nonterm_i ++ sigma ++ ['#']
    in foldl (\acc x -> acc && x `elem` union) True (snd rule_i)

-- Funkcia implementujúca algoritmus 4.1 z opory TIN
algorithm41 :: [Symbol] -> BKG -> [Symbol]
algorithm41 n_prev g
    | n_prev == n_curr = n_curr -- ukončujúca podmienka algoritmu
    | otherwise = algorithm41 n_curr g
        where n_curr = foldl (\acc (x,y) -> if isRuleInUnion (x,y) n_prev (terminals g) && x `notElem` n_prev then acc ++ [x] else acc) n_prev (rules g)-- vypočítať N_i podľa alg 4.1 krok 2

--
isRuleFromSet :: Rule -> [Symbol] -> [Symbol] -> Bool
isRuleFromSet r n t =
    let union = n ++ t ++ ['#']
    in fst r `elem` n && foldl (\acc x -> acc && x `elem` union) True (snd r)

getAllRulesFromSet :: [Rule] -> [Symbol] -> [Symbol] -> [Rule]
getAllRulesFromSet rs n t = filter (\x -> isRuleFromSet x n t) rs

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | x `elem` list2 = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

-- get mutual elements from lists
mutual :: Eq a => [a] -> [a] -> [a]
mutual [] _ = []
mutual (x:xs) ys
    | x `elem` ys = x : mutual xs ys
    | otherwise   = mutual xs ys

-- TODO - fakt vymyslieť lepšie mená
algorithm42Helper :: [Symbol] -> BKG -> [Symbol]
algorithm42Helper v_prev g
    | v_prev == v_curr = v_curr -- end of algorithm
    | otherwise = algorithm42Helper v_curr g
        where v_curr = removeDuplicates (concatMap snd (filter (\(x,_) -> x `elem` v_prev) (rules g)) ++ v_prev)

algorithm42 :: BKG -> BKG
algorithm42 g =
    let v_t   = algorithm42Helper [startSymbol g] g
        n_new = mutual v_t (nonterminals g) -- new nonterminals
        t_new = mutual v_t (terminals g) -- new terminals
        r_new = getAllRulesFromSet (rules g) v_t v_t-- new rules
    in BKG { nonterminals=n_new
           , terminals=t_new
           , rules=r_new
           , startSymbol=startSymbol g
           }

algorithm43Partial :: BKG -> BKG
algorithm43Partial g =
    let n_t = algorithm41 [] g
        p1  = getAllRulesFromSet (rules g) n_t (terminals g)
        n_new = if startSymbol g `elem` n_t then n_t else startSymbol g : n_t
    in BKG { nonterminals=n_new
           , terminals=terminals g
           , rules=p1
           , startSymbol=startSymbol g
           }

algorithm43Full :: BKG -> BKG
algorithm43Full = algorithm42 . algorithm43Partial
