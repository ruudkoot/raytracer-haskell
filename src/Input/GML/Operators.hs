{-# OPTIONS_GHC -XFlexibleInstances #-}

module Input.GML.Operators (operators,runOp,Operator) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Map hiding (map)

import Data.Typeable

import Shared.Vector
import Shared.RenderBase

import Input.GML.AST hiding (State)
import Input.GML.Render
import Input.GML.Shaders

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
popo::Op Object
popl::Op Light
popc::Op Closure
pops::Op String

push::Value -> Op Value
pushi::Int -> Op Value
pushr::Double -> Op Value
pushp::Point -> Op Value
pushb::Bool -> Op Value
pusho::Object -> Op Value
pushl::Light -> Op Value


-- * Pop
pop = let cf []     = lift (throwError "Empty stack")
          cf (x:xs) = put xs >> return x
      in get >>= cf
      
popt :: (Value -> Maybe a) -> String -> Op a
popt match etype 
  = pop >>= maybe ((lift.throwError) ("Expected type: " ++ etype)) return . match
      
popi = popt (\x -> case x of (BaseValue (Int  i)) -> Just i; _ -> Nothing)
       "Int"
popr = popt (\x -> case x of (BaseValue (Real r)) -> Just r; _ -> Nothing)
       "Real"
popp = popt (\x -> case x of (Point p)   -> Just p; _ -> Nothing)
       "Point"
popa = popt (\x -> case x of (Array a)   -> Just a; _ -> Nothing)
       "Array"
popo = popt (\x -> case x of (Object a)  -> Just a; _ -> Nothing)
       "Object"
popl = popt (\x -> case x of (Light a)   -> Just a; _ -> Nothing)
       "Light"
popc = popt (\x -> case x of (Closure a) -> Just a; _ -> Nothing)
       "Closure"
pops = popt (\x -> case x of (BaseValue (String s)) -> Just s; _ -> Nothing)
            "String"

-- * Push

push v = do cs <- get
            put (v:cs)
            return v

pushi = push.BaseValue .Int
pushr = push.BaseValue .Real
pushp = push.Point
pushb = push.BaseValue .Boolean
pusho = push.Object
pushl = push.Light
pushR = push.Render

type Operator = Op Value

ii   :: (Int                        -> Int                         ) -> Operator
iii  :: (Int    -> Int              -> Int                         ) -> Operator
rrr  :: (Double -> Double           -> Double                      ) -> Operator
rr   :: (Double                     -> Double                      ) -> Operator
ri   :: (Double                     -> Int                         ) -> Operator
ir   :: (Int                        -> Double                      ) -> Operator
pr   :: (Point                      -> Double                      ) -> Operator
rrrp :: (Double -> Double -> Double -> Point                       ) -> Operator
iib  :: (Int    -> Int              -> Bool                        ) -> Operator
rrb  :: (Double -> Double           -> Bool                        ) -> Operator
aiv  :: (Array  -> Int              -> Value                       ) -> Operator
ai   :: (Array                      -> Int                         ) -> Operator
co   :: (Closure                    -> Object                      ) -> Operator
orrro:: (Object -> Double -> Double -> Double -> Object            ) -> Operator
oro  :: (Object -> Double           -> Object                      ) -> Operator
paoiriisR :: (Point -> Array -> Object -> Int -> Double -> Int -> Int -> String -> GMLRender) -> Operator

ii op   = (op <$> popi)                             >>= pushi
iii op  = (op <$> popi <*> popi)                    >>= pushi
rr op   = (op <$> popr)                             >>= pushr
rrr op  = (op <$> popr <*> popr)                    >>= pushr
ri op   = (op <$> popr)                             >>= pushi
ir op   = (op <$> popi)                             >>= pushr
pr op   = (op <$> popp)                             >>= pushr
rrrp op = (op <$> popr <*> popr <*> popr)           >>= pushp
iib op  = (op <$> popi <*> popi)                    >>= pushb
rrb op  = (op <$> popr <*> popr)                    >>= pushb
aiv op  = (op <$> popa <*> popi)                    >>= push
ai op   = (op <$> popa)                             >>= pushi
co op   = (op <$> popc)                             >>= pusho
orrro op= (op <$> popo <*> popr <*> popr <*> popr)  >>= pusho
oro op  = (op <$> popo <*> popr)                    >>= pusho
ppl op  = (op <$> popp <*> popp)                    >>= pushl
ppprrl op = (op <$> popp <*> popp <*> popp <*> popr <*> popr) >>= pushl
ooo op  = (op <$> popo <*> popo)                    >>= pusho
paoiriisR op = (op <$> popp <*> popa <*> popo <*> popi <*> popr <*> popi <*> popi <*> pops) >>= pushR

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
                     , ( "getx"  ,   pr getX3D                 ) -- points
                     , ( "gety"  ,   pr getY3D                 )
                     , ( "getz"  ,   pr getZ3D                 )
                     , ( "point" , rrrp (\x y z -> Vector3D (x, y, z)))
                     , ( "get"   ,  aiv (!!)                   ) -- arrays
                     , ( "length",   ai length                 )
                     , ( "sphere",   co (Simple Sphere .gmlShader)) -- Primitive Objects
                     , ( "cube"  ,   co (Simple Cube .gmlShader)   )
                     , ( "cylinder", co (Simple Cylinder .gmlShader))
                     , ( "cone"  ,   co (Simple Cone .gmlShader) )
                     , ( "plane" ,    co (Simple Plane .gmlShader))
                     , ( "translate",orrro Translate           ) --Transformations
                     , ( "scale" ,   orrro  Scale              ) 
                     , ( "uscale",   oro  UScale               )
                     , ( "rotatex",  oro RotateX               )
                     , ( "rotatey",  oro RotateY               )
                     , ( "rotatez",  oro RotateZ               )
                     , ( "light"  ,  ppl DirectLight           ) --Lights
                     , ( "pointlight", ppl PointLight          )                            
                     , ( "spotlight", ppprrl SpotLight         )
                     , ( "union"  ,  ooo Union                 ) --Boolean operators
                     , ( "intersect", ooo Intersect            )
                     , ( "difference", ooo Difference          )
                     , ( "render", paoiriisR renderF         )
                     ]

--Convert light array types
renderF::Point -> Array -> Object -> Int -> Double -> Int -> Int -> String -> GMLRender
renderF p a = GMLRender p (map (\(Light l) -> l) a)

runOp::(String,Operator) -> Stack -> Stack
runOp (nm,op) st = let er e = error ("error running operator "++nm++": "++e)
                   in either er id (execStateT op st)

clampf :: Double -> Double
clampf r1 | r1 < 0.0  = 0.0
          | r1 > 1.0  = 1.0
          | otherwise = r1

