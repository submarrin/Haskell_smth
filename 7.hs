import Data.Maybe
import Control.Applicative
import Control.Monad

import System.IO

-- 1
{-
frozenSum :: [Int] -> [(Int->Int)]
frozenSum =  fmap (+)
frozenSum' :: [Int] -> [(Int->Int)]
frozenSum' = ((+) <$>)

-- 2
applySum :: Int -> [Int] -> [Int]
applySum k lst = (frozenSum lst) <*> [k]

-- 3
algebraSum :: [Int] -> [Int] -> [Int]
algebraSum lst1 lst2 = (frozenSum lst1) <*> lst2

-- 4
carteProduct :: [Int] -> [Int] -> [(Int,Int)]
carteProduct lst1 lst2 = (<$> (,) lst1) <*> lst2
-}

-- 5
maybeAdd :: [Maybe Int] -> [Maybe Int]
maybeAdd = (<$>) . (+ 3) $ (<$>)