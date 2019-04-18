-- 3
maxString :: [[Char]] -> [Char]
maxString = foldl1 (zipWith max)

-- 4
sequence' :: Integer -> [Integer]
sequence' n = concatMap int2lst [1..n]
int2lst :: Integer -> [Integer]
int2lst n = reverse $ map (`mod` 10) $ takeWhile (/=0) $ iterate (`div` 10)  n

makeSequence :: Int -> [Integer]
makeSequence n =
    map fst .
    take n  $
    iterate (\(x,y) -> (x + fromIntegral y * 2, y + 1)) 
    (1,2)