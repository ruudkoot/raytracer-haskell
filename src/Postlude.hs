module Postlude where

infixr 0 #
infix  4 ~=

-- | Flipped application
--
(#) :: a -> (a -> b) -> b
d # f = f d

-- | Approxiately equals
--
(~=) :: (Fractional t, Ord t) => t -> t -> Bool
x ~= y = abs (x - y) < 0.001

clampf :: Double -> Double
clampf r1 | r1 < 0.0  = 0.0
          | r1 > 1.0  = 1.0
          | otherwise = r1

-- | Solves an equation of the form:
--     @ax^2 + bx + c = 0@
--


solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c = let discr  = b ^ 2 - 4 * a * c
                           abc op = (-b `op` sqrt discr) / (2 * a)
                        in case compare discr 0.0 of 
                             LT -> [] 
                             EQ -> [-b / (2 * a)]
                             GT -> [abc (-), abc (+)]

-- | Order a tuple.
sort2 :: (Ord a) => (a,a) -> (a,a)
sort2 (x,y) = if x < y then (x,y) else (y,x)

