
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
                        "P = {" ++  intercalate "," (map (\(x, y) -> "(" ++ [x] ++ "," ++ y ++ ")") r) ++ "}\n" ++
                        "S = " ++ show s

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

-- example grammar
g :: BKG
g = BKG { nonterminals="SAB"
        , terminals="abcd"
        , rules=[('S', "#"), ('S', "AB"), ('A', "aAb"), ('A', "ab"), ('B', "cBd"), ('B', "cd")]
        , startSymbol='S'}

r = [('S', "#"), ('S', "AB"), ('A', "aAb"), ('A', "ab"), ('B', "cBd"), ('B', "cd")]

main = do
    putStrLn "FLP - Simplify"