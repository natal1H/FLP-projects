-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Main.hs
-- Popis:   Hlavný súbor

import System.Environment
import ParseInput

main :: IO ()
main = do
    (command:args) <- getArgs -- command should be -i/-1/-2
    let (Just action) = lookup command dispatch -- lookup command in dispatch list
    action args -- will return IO action
