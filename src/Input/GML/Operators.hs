{-# OPTIONS_GHC -XFlexibleInstances #-}

module Input.GML.Operators (operators,runOp,Operator) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Control.Monad
import Data.Map hiding (map)
import Data.Typeable

import Shared.Vector
import Shared.RenderBase

import Input.GML.AST hiding (State)
import qualified Input.GML.Render as Render
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
popo::Op Render.Object
popl::Op Light
popc::Op Closure
pops::Op String

push::Value -> Op Value
pushi::Int -> Op Value
pushr::Double -> Op Value
pushp::Point -> Op Value
pushb::Bool -> Op Value
pusho::Render.Object -> Op Value
pushl::Light -> Op Value

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
popo = let cf (Object a) = return a
           cf _          = lift (throwError "Expected Type: Object")
       in pop >>= cf
popl = let cf (Light a) = return a
           cf _         = lift (throwError "Expected Type: Light")
       in pop >>= cf
popc = let cf (Closure a) = return a
           cf _           = lift (throwError "Expected Type: Closure")
       in pop >>= cf
pops = let cf (BaseValue (String s)) = return s
           cf _                      = lift (throwError "Expected Type: String")
       in pop >>= cf

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
co   :: (Closure                    -> Render.Object               ) -> Operator
orrro:: (Render.Object -> Double -> Double -> Double -> Render.Object     ) -> Operator
oro  :: (Render.Object -> Double           -> Render.Object               ) -> Operator
paoiriisR :: (Point -> Array -> Render.Object -> Int -> Double -> Int -> Int -> String -> Render.Render) -> Operator

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
                     , ( "sphere",   co (Render.Simple Sphere .gmlShader)) -- Primitive Objects
                     , ( "cube"  ,   co (Render.Simple Cube .gmlShader)   )
                     , ( "cylinder", co (Render.Simple Cylinder .gmlShader))
                     , ( "cone"  ,   co (Render.Simple Cone .gmlShader) )
                     , ( "plane" ,    co (Render.Simple Plane .gmlShader))
                     , ( "translate",orrro Render.Translate           ) --Transformations
                     , ( "scale" ,   orrro  Render.Scale              ) 
                     , ( "uscale",   oro  Render.UScale               )
                     , ( "rotatex",  oro Render.RotateX               )
                     , ( "rotatey",  oro Render.RotateY               )
                     , ( "rotatez",  oro Render.RotateZ               )
                     , ( "light"  ,  ppl DirectLight           ) --Lights
                     , ( "pointlight", ppl PointLight          )                            
                     , ( "spotlight", ppprrl SpotLight         )
                     , ( "union"  ,  ooo Render.Union                 ) --Boolean operators
                     , ( "intersect", ooo Render.Intersect            )
                     , ( "difference", ooo Render.Difference          )
                     , ( "render", paoiriisR renderF         )
                     ]

--Convert light array types
renderF::Point -> Array -> Render.Object -> Int -> Double -> Int -> Int -> String -> Render.Render
renderF p a = Render.Render p (map (\(Light l) -> l) a)

runOp::(String,Operator) -> Stack -> Stack
runOp (nm,op) st = let er e = error ("error running operator "++nm++": "++e)
                   in either er id (execStateT op st)

clampf :: Double -> Double
clampf r1 | r1 < 0.0  = 0.0
          | r1 > 1.0  = 1.0
          | otherwise = r1

