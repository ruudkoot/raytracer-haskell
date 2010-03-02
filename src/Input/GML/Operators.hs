module Input.GML.Operators (operators) where

import Data.Map
import Input.GML.AST

type Operator = Stack -> (Value, Stack)

ii   :: (Int                        -> Int                         ) -> Operator
iii  :: (Int    -> Int              -> Int                         ) -> Operator
rrr  :: (Double -> Double           -> Double                      ) -> Operator
rr   :: (Double                     -> Double                      ) -> Operator
ri   :: (Double                     -> Int                         ) -> Operator
ir   :: (Int                        -> Double                      ) -> Operator
pr   :: ((Double,  Double,   Double)-> Double                      ) -> Operator
--rrrp :: (Double -> Double -> Double -> (Double -> Double -> Double)) -> Operator
iib  :: (Int    -> Int              -> Bool                        ) -> Operator
rrb  :: (Double -> Double           -> Bool                        ) -> Operator

ii   f = \(BaseValue (Int  i1)                                             : ss) -> (BaseValue (Int     (f i1      )), ss)
iii  f = \(BaseValue (Int  i1) : BaseValue (Int  i2)                       : ss) -> (BaseValue (Int     (f i1 i2   )), ss)
rrr  f = \(BaseValue (Real r1) : BaseValue (Real r2)                       : ss) -> (BaseValue (Real    (f r1 r2   )), ss)
rr   f = \(BaseValue (Real r1)                                             : ss) -> (BaseValue (Real    (f r1      )), ss)
ri   f = \(BaseValue (Real r1)                                             : ss) -> (BaseValue (Int     (f r1      )), ss)
ir   f = \(BaseValue (Int  i1)                                             : ss) -> (BaseValue (Real    (f i1      )), ss)
pr   f = \(Point           p1                                              : ss) -> (BaseValue (Real    (f p1      )), ss)
rrrp f = \(BaseValue (Real r1) : BaseValue (Real r2) : BaseValue (Real r3) : ss) -> (Point              (f r1 r2 r3) , ss)
iib  f = \(BaseValue (Int  i1) : BaseValue (Int  i2)                       : ss) -> (BaseValue (Boolean (f i1 i2   )), ss)
rrb  f = \(BaseValue (Real r1) : BaseValue (Real r2)                       : ss) -> (BaseValue (Boolean (f r1 r2   )), ss)
aiv  f = \(Array           a1  : BaseValue (Int  i2)                       : ss) -> (                    f a1 i2     , ss)
ai   f = \(Array           a1                                              : ss) -> (BaseValue (Int     (f a1)      ), ss)
                                

-- applicative??
operators :: Map String Operator
operators = fromList [ ( "addi"  ,  iii (+)                    ) -- numbers
                     , ( "addf"  ,  rrr (+)                    )
                     , ( "acos"  ,   rr acos                   )
                     , ( "asin"  ,   rr asin                   )
                     , ( "clampf",   rr clampf                 )
                     , ( "cos"   ,   rr cos                    )
                     , ( "divi"  ,  iii div                    )
                     , ( "divf"  ,  rrr (/)                    )
                     , ( "eqi"   ,  iib (==)                   )
                     , ( "eqf"   ,  rrb (==)                   )
                     , ( "floor" ,   ri floor                  )
                     , ( "frac"  ,   rr (snd . properFraction) ) -- ???
                     , ( "lessi" ,  iib (<)                    )
                     , ( "lessf" ,  rrb (<)                    )
                     , ( "modi"  ,  iii mod                    )
                     , ( "muli"  ,  iii (*)                    )
                     , ( "mulf"  ,  rrr (*)                    )
                     , ( "negi"  ,   ii negate                 )
                     , ( "negf"  ,   rr negate                 )
                     , ( "real"  ,   ir fromIntegral           )
                     , ( "sin"   ,   rr sin                    )
                     , ( "sqrt"  ,   rr sqrt                   )
                     , ( "subi"  ,  iii (-)                    )
                     , ( "subf"  ,  rrr (-)                    )
                     , ( "getx"  ,   pr getx                   ) -- points
                     , ( "gety"  ,   pr gety                   )
                     , ( "getz"  ,   pr getz                   )
                     , ( "point" , rrrp (,,)                   )
                     , ( "get"   ,  aiv (!!)                   ) -- arrays
                     , ( "lenght",   ai length                 ) ]

clampf :: Double -> Double
clampf r1 | r1 < 0.0  = 0.0
          | r1 > 1.0  = 1.0
          | otherwise = r1

getx :: Point -> Double
getx (x, _, _) = x

gety :: Point -> Double
gety (_, y, _) = y

getz :: Point -> Double
getz (_, _, z) = z

