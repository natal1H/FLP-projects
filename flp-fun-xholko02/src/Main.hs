-- Projekt: FLP - simplify-bkg
-- Autor:   Natália Holková (xholko02)
-- Dátum:   -.-.2022
-- Súbor:   Main.hs
-- Popis:   Hlavný súbor

import System.Environment ( getArgs )
import ParseInput ( validArguments, dispatch )

-- Hlavná funkcia main
main :: IO ()
main = do
    args <- getArgs -- argument by mal byť -i/-1/-2

    let usagemsg = "Usage example: ./flp21-fun -1 fileLocation"
    case validArguments args of
        True -> let (Just action) = lookup (head args) dispatch -- vyhľadať príkaz v dispatch zozname
                in action (tail args) -- vypíše vrátenú IO String
        False -> error $ "Invalid arguments!" ++ usagemsg
