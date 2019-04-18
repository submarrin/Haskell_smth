import System.IO
import Control.Monad
--import System.Random
import Data.List
import Control.Monad.Trans.State

-- 1
main = do
  lst <- (map read . words) <$> 
                 readFile "input.txt" :: IO [Int]
  let
    res = reverse (sort lst)
  writeFile "output.txt" $ (unwords $ map show res)
  
-- 3

main3 = guess start 1 1000000 where
    guess a b = do
        start <- getStdRandom (randomR (a,b)) :: IO Int
        putStrLn "If your number greater than " ++ start ++ " write '>'." 
        "If smaller write '<'. If it is equal to " ++ start ++ " write '='"
        command <- getLine
        case command of
                "=" -> "Ваше число = " ++ show start
                "<" -> guess 1 start
                ">" -> guess start 1000000