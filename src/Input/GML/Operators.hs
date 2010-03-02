module Input.GML.Operators (operators) where

import Data.Map
import Input.GML.AST

type Operator = String -> Stack -> (Value, Stack)

iii  f = \(BaseValue (Int  i1) : BaseValue (Int  i2)                       : ss) -> (BaseValue (Int     (f i1 i2   ), ss)
rrr  f = \(BaseValue (Real r1) : BaseValue (Real r2)                       : ss) -> (BaseValue (Real    (f r1 r2   ), ss)
rr   f = \(BaseValue (Real r1)                                             : ss) -> (BaseValue (Real    (f r1      ), ss)
ri   f = \(BaseValue (Real r1)                                             : ss) -> (BaseValue (Int     (f r1      ), ss)
ir   f = \(BaseValue (Int  i1)                                             : ss) -> (BaseValue (Real    (f i1      ), ss)
pr   f = \(Point           p1                                              : ss) -> (BaseValue (Real    (f p1      ), ss)
rrrp f = \(BaseValue (Real r1) : BaseValue (Real r2) : BaseValue (Real r3) : ss) -> (Point              (f r1 r2 r3), ss)
iib  g = \(BaseValue (Int  i1) : BaseValue (Int  i2)                       : ss) -> (BaseValue (Boolean (f i1 i2   ), ss)
rrb  g = \(BaseValue (Real r1) : BaseValue (Real r2)                       : ss) -> (BaseValue (Boolean (f r1 r2   ), ss)
aiv  f = \(Array           a1  : BaseValue (Int  i2)                       : ss) -> (                    f a1 i2    , ss)
ai   f = \(Array           a1                                              : ss) -> (BaseValue (Int     (f a1)      , ss)
                                

-- applicative??
operators :: Map String Operator
operators = fromList [ ( "addi"  ,  iii (+)                    ) -- numbers
                     , ( "addf"  ,  rrr (+)                    )
                     , ( "acos"  ,   rr acos                   )
                     , ( "asin"  ,   rr asin                   )
                     , ( "clampf",   rr clampf                 )
                     , ( "cos"   ,   rr cos                    )
                     , ( "divi"  ,  iii (/)                    ) -- ???
                     , ( "divf"  ,  rrr (/)                    )
                     , ( "eqi"   ,  iii (==)                   )
                     , ( "eqf"   ,  rrr (==)                   )
                     , ( "floor" ,   ri floor                  )
                     , ( "frac"  ,   rr (snd . properFraction) ) -- ???
                     , ( "lessi" ,  iib (<)                    )
                     , ( "lessf" ,  rrb (<)                    )
                     , ( "modi"  ,  iii (%)                    )
                     , ( "muli"  ,  iii (*)                    )
                     , ( "mulf"  ,  rrr (*)                    )
                     , ( "negi"  ,   ii neg                    )
                     , ( "negf"  ,   rr neg                    )
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
clampf r1 = if r1 < 0.0 then 0.0 else if r1 > 1.0 then 1.0 else r1

getx :: Point -> Double
getx (x, _, _) = x

gety :: Point -> Double
gety (_, y, _) = y

getz :: Point -> Double
getz (_, _, z) = z

