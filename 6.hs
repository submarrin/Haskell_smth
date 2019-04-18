class AbelGroup a where
    (.+.) :: a -> a -> a
    (.-.) :: a -> a -> a
    x .-. y = x .+. opposite y
    opposite :: a -> a 
    opposite x = zero .-. x
    zero :: a
    
data TVector = Vector { x :: Double, y :: Double }
  deriving (Show)
  
instance AbelGroup TVector where 
  v1 .+. v2 = Vector (x v1 + x v2) (y v1 + y v2)
  v1 .-. v2 = Vector (x v1 - x v2) (y v1 - y v2)
  opposite (Vector x y) = Vector (-x) (-y)
  zero = Vector 0 0   


  
instance AbelGroup Int where  
  (.+.) = (+) 
  (.-.) = (-) 
  zero = 0 
  
class AbelGroup a => Field a where
  (.*.) :: a -> a -> a
  (./.) :: a -> a -> a
  unit :: a
  reciprocate :: a -> a  
  reciprocate x = unit ./. x
   
instance AbelGroup Double where
  (.+.) = (+) 
  (.-.) = (-) 
  zero = 0 
    
instance Field Double where
  (.*.) = (*)
  unit = 1
  (./.) = (/)
  
-- 1

class AbelGroup a => LinearSpace a where
    (.**.) :: Double -> a -> a

instance LinearSpace TVector where
    (.**.) a v = Vector (a*x v) (a*y v)
    
class LinearSpace a => HilbertSpace a where
    (.%.) :: a -> a -> Double
     
--2
    
instance HilbertSpace TVector where
    (.%.) v1 v2 = (x v1)*(x v2) + (y v1)*(y v2)

-- 4 
class MyMonoid a where
    (.++.)  :: a -> a -> a
    neutral :: a
    
-- 5  
instance MyMonoid [a] where 
    (.++.)  = (++)
    neutral = [] 

class MyMonoid a => AdditiveGroup a where
    (.^.) :: a -> a -> a
    x .^. y = x .++. oppositeAdd y
    oppositeAdd :: a -> a