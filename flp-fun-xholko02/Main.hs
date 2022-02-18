import System.IO
import System.Directory
import System.Environment
import ParseInput

main = do
    (command:args) <- getArgs -- command should be -i/-1/-2
    let (Just action) = lookup command dispatch -- lookup command in dispatch list
    action args -- will return IO action
