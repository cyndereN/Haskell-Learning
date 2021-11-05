import System.IO
import System.Random

main = do
       handle <- openFile "enable1.txt" ReadMode
       contents <- hGetContents handle
       gen <- getStdGen
       let words =map init (lines contents)
           (n, _) = randomR (0,(length words)-1) gen :: (Int, StdGen)
       play(words !! n)
       hClose handle
play word = do
            putStrLn("The word is " ++ word ++ ".")