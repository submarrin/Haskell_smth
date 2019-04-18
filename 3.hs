import Data.List

-- myZip
myZip :: ([a],[b]) -> [(a,b)]
myZip ([],_)           = []
myZip (_,[])           = []
myZip ((a:aT), (b:bT)) = (a,b) : (myZip (aT,bT))

-- sumSquares
sumSquaresA , sumSquaresB , sumSquaresC :: (Num a) => [a] -> a
sumSquaresA [] = 0
sumSquaresA (a:aT) = a*a + sumSquaresA aT

sumSquaresB lst =
    helpCount 0 lst where
    helpCount res [] = res
    helpCount res (a:aT) = helpCount (res + a*a) aT

sumSquaresC = sum . map (^2)


-- findDot

findDot :: [(Double,Double)] -> (Double,Double)
findDot = foldl1 func where
    func (x,y) (xCur,yCur)
        | (angle < angleCur) || (angle == angleCur && dist > distCur) = (x,y)
        | otherwise = (xCur,yCur) where
        angle     = findAngle x y
        angleCur  = findAngle xCur yCur
        dist      = sqrt $ x^2 + y^2
        distCur   = sqrt $ xCur^2 + yCur^2

findAngle :: Double -> Double -> Double
findAngle x y
    | quadrant (x,y) == 1 || quadrant (x,y) == 2 = angle
    | otherwise = angle + 2*pi where
    angle = atan2 y x

quadrant :: (Double,Double) -> Int
quadrant (x,y)
    | x >= 0 && y >= 0 = 1
    | x <= 0 && y >= 0 = 2
    | x < 0 && y < 0   = 3
    | x > 0 && y < 0   = 4

-- greaterMedian

greaterMedian :: [Integer] -> [Integer]
greaterMedian lst = filter (> median) lst where
    median = findMed (sort lst)

findMed :: [Integer] -> Integer
findMed lst =
    let 
    len = length lst
    in
    if even len then lst !! (len `div` 2 - 1)
    else lst !! (len `div` 2)
