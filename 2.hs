-- 1 doublefact
doublefact :: Double -> Double
doublefact 0 = 1
doublefact n = n * doublefact (n-2)

-- 2 sumDig
sumDig :: Integer -> Integer
sumDig 0 = 0
sumDig n = (n `mod` 10) + sumDig (n `div` 10)

-- 3 dist

dist :: Double -> Double -> Double
dist x y
    | x >= 0 && y >= 0 = caseOne x y
    | x < 0 && y >= 0  = caseTwo x y
    | abs x >= 5       = caseThree x y
    | otherwise        = caseFour x y

caseOne :: Double -> Double -> Double
caseOne x y
    | y + x < 5 = 0
    | x == 0 = abs y - 5
    | y == 0 = abs x - 5
    | otherwise = res where
    d1 = abs (y + x - 5)/(sqrt 2)
    d2 = sqrt (x^2 + (y - 5)^2)
    d3 = sqrt ((x - 5)^2 + y^2)
    res = minimum [d1, d2, d3]

caseTwo :: Double -> Double -> Double
caseTwo x y 
    | x^2 + y^2 > 25 = d
    |otherwise = 0 where
    d = sqrt (x^2 + y^2) - 5

caseThree :: Double -> Double -> Double
caseThree x y
    | abs y <= 5 = d1
    | otherwise = d2 where
    d1 = abs x - 5
    d2 = sqrt ((abs x - 5)^2 + (abs y - 5)^2)

caseFour :: Double -> Double -> Double
caseFour x y
    | y >= (-5) = 0
    | otherwise = abs y - 5

-- 4 chooseBest
chooseMax :: Integer -> Integer -> Integer -> Integer
chooseMax a b c
    | a > b && a > c = a
    | b > a && b > c = b
    |otherwise = c
chooseMin :: Integer -> Integer -> Integer -> Integer
chooseMin a b c
    | a < b && a < c = a
    | b < a && b < c = b
    |otherwise = c
chooseBest :: Integer -> Integer -> Integer -> Integer
chooseBest a b c
    | a > 10 && b > 10 && c > 10 = chooseMax a b c
    |otherwise = chooseMin a b c

-- 5

powerArg, power, addPowerArg, addPower, powerSubtractArg, powerSubtract, addSubtPowerArg, addSubtPower :: Integer -> Integer -> Integer

-- additional

subtract3 = flip (-) 3
add1 = (+) 1

-- 5.а power

powerArg x y = x ^ y

power = (^)

-- 5.б addPower

addPowerArg x y = z ^ y where z = x + 1

addPower = (^) . (+ 1)

-- 5.в powerSubtract

powerSubtractArg x y = x ^ (y - 3)

powerSubtract = flip (flip (^) . subtract3)

-- 5.г addSubtPower

addSubtPowerArg x y = (x + 1) ^ (y - 3)

addSubtPower = flip (flip (^) . subtract3) . (+ 1)

-- 6
multiplicate, addMultp, multpSubt, addSubtMultp :: Integer -> Integer

-- 6.а dArg

dArg :: (a -> a-> b) -> a -> b
dArg f x = f x x


-- 6.б multiplicate

multiplicate = dArg (*)

-- 6.в addMultp

addMultp = dArg ((*) . (+ 1))

-- 6.г multpSubt

multpSubt = dArg (flip (flip (*) . subtract3))

-- 6.д addSubtMultp

addSubtMultp = multpSubt . (+ 1)