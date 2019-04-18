import Data.List

-- 4.4
dichotomy :: (Double -> Double) -> Double -> Double -> Double -> Double
dichotomy func a b eps =
    root $ head $ 
    dropWhile (\(x,y) -> y - x >= eps) $
    iterate help (a,b) where
    root (x,y) = (x + y) / 2
    help (x,y) | signum (func x) == signum (func z) = (z,y)
               | otherwise = (x,z) where z = (x + y) / 2
-- 4.5
sumSerie :: (Int -> Double) -> Double -> Double
sumSerie rule eps =
    sum $ 
    takeWhile (\x -> abs x >= eps) $
    map (\i -> help (fromIntegral i)) [1..] where
        help i | even i = rule i | otherwise = 0 - rule i

rule1 :: Int -> Double
rule1 n = 1 / (fromIntegral n)
rule2 n = 1 / (m*m) where m = fromIntegral n

-- 4.6
listDigets :: Int -> [Integer]
listDigets n = concatMap (reverse . separateDigets) [1..n]
separateDigets :: Int -> [Integer]
separateDigets n = 
    map (\m -> mod m 10) $ 
    takeWhile (\m -> m /=0) $ iterate (\m -> div m 10) m 
    where m = fromIntegral n