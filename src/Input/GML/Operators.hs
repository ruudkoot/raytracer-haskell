{-# OPTIONS_GHC -XFlexibleInstances #-}

module Input.GML.Operators (operators,runOp,Operator) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Control.Monad
import Data.Map
import Input.GML.AST hiding (State)

type Op = StateT Stack (Either String) 

instance Applicative (Either String) where
    pure  = return
    (<*>) = ap

instance (Monad f, Applicative f) => Applicative (StateT r f) where
    pure  = return
    (<*>) = ap

pop::Op Value
popi::Op Int
popr::Op Double
popp::Op Point
popa::Op Array

push::Value -> Op Value
pushi::Int -> Op Value
pushr::Double -> Op Value
pushp::Point -> Op Value
pushb::Bool -> Op Value

pop = let cf []     = lift (throwError "Empty stack")
          cf (x:xs) = put xs >> return x
      in get >>= cf
popi = let cf (BaseValue (Int i)) = return i
           cf _                   = lift (throwError "Expected Type: Int")
       in pop >>= cf
popr = let cf (BaseValue (Real r)) = return r
           cf _                    = lift (throwError "Expected type: Real")
       in pop >>= cf
popp = let cf (Point p) = return p
           cf _         = lift (throwError "Expected Type: Point")
       in pop >>= cf
popa = let cf (Array a) = return a
           cf _         = lift (throwError "Expected Type: Array")
       in pop >>= cf

push v = do cs <- get
            put (v:cs)
            return v
pushi = push.BaseValue .Int
pushr = push.BaseValue .Real
pushp = push.Point
pushb = push.BaseValue .Boolean

type Operator = Op Value

ii   :: (Int                        -> Int                         ) -> Operator
iii  :: (Int    -> Int              -> Int                         ) -> Operator
rrr  :: (Double -> Double           -> Double                      ) -> Operator
rr   :: (Double                     -> Double                      ) -> Operator
ri   :: (Double                     -> Int                         ) -> Operator
ir   :: (Int                        -> Double                      ) -> Operator
pr   :: ((Double,  Double,   Double)-> Double                      ) -> Operator
rrrp :: (Double -> Double -> Double -> (Double, Double, Double)    ) -> Operator
iib  :: (Int    -> Int              -> Bool                        ) -> Operator
rrb  :: (Double -> Double           -> Bool                        ) -> Operator


ii op   = (op <$> popi)                   >>= pushi
iii op  = (op <$> popi <*> popi)          >>= pushi
rr op   = (op <$> popr)                   >>= pushr
rrr op  = (op <$> popr <*> popr)          >>= pushr
ri op   = (op <$> popr)                   >>= pushi
ir op   = (op <$> popi)                   >>= pushr
pr op   = (op <$> popp)                   >>= pushr
rrrp op = (op <$> popr <*> popr <*> popr) >>= pushp
iib op  = (op <$> popi <*> popi)          >>= pushb
rrb op  = (op <$> popr <*> popr)          >>= pushb
aiv op  = (op <$> popa <*> popi)          >>= push
ai op   = (op <$> popa)                   >>= pushi

{-
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
-}

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
                     , ( "lessi" ,  iib (>)                    )
                     , ( "lessf" ,  rrb (>)                    )
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

runOp::(String,Operator) -> Stack -> Stack
runOp (nm,op) st = let er e = error ("error running operator "++nm++": "++e)
                   in either er id (execStateT op st)

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

