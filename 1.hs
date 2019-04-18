-- fact
fact :: Integer -> Integer
fact n =
    if n == 0 then 1
        else n * fact (n-1)

-- GCD
gcd' :: Integer -> Integer -> Integer
gcd' a b = 
   if b == 0 then a
   else gcd' b (mod a b)

-- isPrime
isPrime :: Integer -> Bool
isPrime p =
    if p == 1 then False
        else helpPrime p 2

helpPrime :: Integer -> Integer -> Bool
helpPrime m i =
    if m >= i^2 then
    let 
    k = mod m i
    in
    if k /= 0 then helpPrime m (i+1)
else False
else True
-- reverseNum
reverseNum :: Integer -> Integer
reverseNum n =
    helpRevNum n 0
    where
    helpRevNum 0 rev = rev
    helpRevNum n rev =
        let
        k = n `mod` 10
        impr = n `div` 10
        new = rev * 10 + k
        in
        helpRevNum impr new
-- findRoots
findRoots :: Integer -> Integer -> Integer -> Double
findRoots a0 b0 c0 =
    if a == 0 then (-c)/b else
    if d < 0 then error "Уравнение корней не имеет"
        else if d == 0 then (- b) / (2 * a)
            else ( (- b) + sqrt d ) / (2 * a)
    where
    a = fromIntegral a0
    b = fromIntegral b0
    c = fromIntegral c0
    d = b * b - 4 * a * c
-- roots
roots :: (Double -> Double) -> Double -> Double -> Double -> Double
roots func a b eps =
    if ( abs (b-a) < eps || func x == 0 ) then x
    else
        if signum (func a) /= signum (func x) then
            roots func a x eps
        else
            roots func x b eps
    where x = (a+b) / 2
func0 :: Double -> Double
func0 x = y
    where y = x*x + x
func1 :: Double -> Double
func1 x = y
    where y = x**3