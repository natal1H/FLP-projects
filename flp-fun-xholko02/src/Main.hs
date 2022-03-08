-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Main.hs
-- Popis:   Hlavný súbor

import System.Environment ( getArgs )
import ParseInput ( dispatch )

-- Hlavná funkcia main
main :: IO ()
main = do
    (command:args) <- getArgs -- argument by mal byť -i/-1/-2
    let (Just action) = lookup command dispatch -- vyhľadať príkaz v dispatch zozname
    action args -- vypíše vrátenú IO String
