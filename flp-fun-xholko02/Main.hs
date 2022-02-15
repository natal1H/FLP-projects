
import System.Environment
import System.Directory
import System.IO
import Data.List

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
   show (BKG n t p s) = "BKG = (N, \x03A3, P, S)\n" ++
                        "N = {" ++ intercalate "," (map (:[]) n) ++ "}\n" ++
                        "\x03A3 = {" ++ intercalate "," (map (:[]) t) ++ "}\n" ++
                        "P = {" ++  intercalate "," (map (\(x, y) -> "(" ++ [x] ++ "," ++ y ++ ")") p) ++ "}\n" ++
                        "S = " ++ [s]

{-- 
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
--}


-- Algoritmus 1
--algoritmus1 :: BKG -> [Symbol]
--algoritmus1 = ...

-- example grammar
g1 :: BKG
g1 = BKG { nonterminals="SAB"
        , terminals="abcd"
        , rules=[('S', "#"), ('S', "AB"), ('A', "aAb"), ('A', "ab"), ('B', "cBd"), ('B', "cd")]
        , startSymbol='S'}

g2 :: BKG
g2 = BKG { nonterminals="SAB"
        , terminals="ab"
        , rules=[('S', "A"), ('S', "a"), ('A', "AB"), ('B', "b")]
        , startSymbol='S'}

r1 = [('S', "#"), ('S', "AB"), ('A', "aAb"), ('A', "ab"), ('B', "cBd"), ('B', "cd")]

-- Funkcia pre Alg 4.1 - kontrola či alpha in (Ni-1 union Sigma)*
-- TODO - prerobiť, že vstup iba alpha, nie A->alpha, lebo isRuleFromSet kontroluje aj A
isRuleInUnion :: Rule -> [Symbol] -> [Symbol] -> Bool
isRuleInUnion rule_i nonterm_i sigma =
    let union = nonterm_i ++ sigma ++ ['#']
    in foldl (\acc x -> acc && x `elem` union) True (snd rule_i)

-- funkcia z Alg 4.1.
alg41Helper :: [Symbol] -> BKG -> [Symbol]
alg41Helper n_prev g = foldl (\acc (x,y) -> if isRuleInUnion (x,y) n_prev (terminals g) && x `notElem` n_prev then acc ++ [x] else acc) n_prev (rules g)

alg41 :: [Symbol] -> BKG -> [Symbol]
alg41 n_prev g
    | n_prev == n_curr = n_curr -- end of algorithm
    | otherwise = alg41 n_curr g
        where n_curr = foldl (\acc (x,y) -> if isRuleInUnion (x,y) n_prev (terminals g) && x `notElem` n_prev then acc ++ [x] else acc) n_prev (rules g)-- vypočítať N_i podľa alg 4.1 krok 2

isRuleFromSet :: Rule -> [Symbol] -> [Symbol] -> Bool
isRuleFromSet r n t =
    let union = n ++ t ++ ['#']
    in fst r `elem` n && foldl (\acc x -> acc && x `elem` union) True (snd r)

getAllRulesFromSet :: [Rule] -> [Symbol] -> [Symbol] -> [Rule]
getAllRulesFromSet rs n t = filter (\x -> isRuleFromSet x n t) rs

alg43_1 :: BKG -> BKG
alg43_1 g =
    let n_t = alg41 [] g
        p1  = getAllRulesFromSet (rules g) n_t (terminals g)
        n_new = if startSymbol g `elem` n_t then n_t else startSymbol g : n_t
    in BKG { nonterminals=n_new
           , terminals=terminals g
           , rules=p1
           , startSymbol=startSymbol g
           }

main = do
    putStrLn "FLP - Simplify"
