module Postlude where

infix  4 ~=

positiveInfinity :: (Fractional t) => t
positiveInfinity =  1.0 / 0.0

negativeInfinity :: (Fractional t) => t
negativeInfinity = -1.0 / 0.0


-- | Approxiately equals
--
(~=) :: (Fractional t, Ord t) => t -> t -> Bool
x ~= y = abs (x - y) < 0.00001


-- | Solves an equation of the form:
--     @ax^2 + bx + c = 0@
--
{-# INLINE solveQuadratic #-}
solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c = let discr  = b * b - 4 * a * c
                           sd = sqrt discr
                           abc op = (-b `op` sd) / (2 * a)
                        in case compare discr 0.0 of 
                             LT -> [] 
                             EQ -> [-b / (2 * a)]
                             GT -> [abc (-), abc (+)]

-- | Order a tuple.
sort2 :: (Ord a) => (a,a) -> (a,a)
sort2 (x,y) = if x < y then (x,y) else (y,x)

