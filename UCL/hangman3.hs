import System.IO
import System.Random

main = do
       handle <- openFile "enable1.txt" ReadMode
       contents <- hGetContents handle
       gen <- getStdGen
       let words = map init (lines contents)
           (n, _) = randomR(0, (length words)-1) gen :: (Int, StdGen)
           word = words !! n
       play word (map(\x -> '_') word)
       hClose handle

play word known = do
                  putStrLn known
                  putStrLn "Enter a letter to guess:"
                  line <- getLine
                  play word (handle(head line) word known)

handle letter word known
       | letter `elem` word = zipWith (\w k -> if w == letter then w else k) word known
       | otherwise          = known