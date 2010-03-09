{-# LANGUAGE FlexibleInstances #-}

module Input.GML.Operators (operators,runOp,Operator) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Data.Maybe
import Data.Map                              hiding (map)

import           Shared.Vector

import qualified Shared.RenderBase as RenderBase
import qualified Base.Light        as Light
import qualified Base.Shape        as Shape

import           Input.GML.AST               hiding (State)
import           Input.GML.Scene
import           Input.GML.Shaders

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

pushi = push . BaseValue . Int
pushr = push . BaseValue . Real
pushp = push . Point
pushb = push . BaseValue . Boolean
pusho = push . Object
pushl = push . Light
pushR = push . Render

type Operator = Op Value

ii     :: (Int                                            -> Int   ) -> Operator
iii    :: (Int    -> Int                                  -> Int   ) -> Operator
rrr    :: (Double -> Double                               -> Double) -> Operator
rr     :: (Double                                         -> Double) -> Operator
ri     :: (Double                                         -> Int   ) -> Operator
ir     :: (Int                                            -> Double) -> Operator
pr     :: (Point                                          -> Double) -> Operator
rrrp   :: (Double -> Double -> Double                     -> Point ) -> Operator
iib    :: (Int    -> Int                                  -> Bool  ) -> Operator
rrb    :: (Double -> Double                               -> Bool  ) -> Operator
aiv    :: (Array  -> Int                                  -> Value ) -> Operator
ai     :: (Array                                          -> Int   ) -> Operator
co     :: (Closure                                        -> Object) -> Operator
orrro  :: (Object -> Double -> Double -> Double           -> Object) -> Operator
oro    :: (Object -> Double                               -> Object) -> Operator
ppl    :: (Point  -> Point                                -> Light ) -> Operator
ppprrl :: (Point  -> Point  -> Point  -> Double -> Double -> Light ) -> Operator
ooo    :: (Object -> Object                               -> Object) -> Operator
paoiriisR :: (Point -> Array -> Object -> Int -> Double -> Int -> Int -> String -> Scene) -> Operator

flip3::(a->b->c->d)->c->b->a->d
flip3 f c b a = f a b c

flip4::(a->b->c->d->e)->d->c->b->a->e
flip4 f d c b a = f a b c d

flip5::(a->b->c->d->e->f)->e->d->c->b->a->f
flip5 f e d c b a = f a b c d e

flip8::(a->b->c->d->e->f->g->h->i)->h->g->f->e->d->c->b->a->i
flip8 ff h g f e d c b a = ff a b c d e f g h

ii        op = (op      <$> popi)                                       >>= pushi
iii       op = (flip op <$> popi <*> popi)                              >>= pushi
rr        op = (op      <$> popr)                                       >>= pushr
rrr       op = (flip op <$> popr <*> popr)                              >>= pushr
ri        op = (op      <$> popr)                                       >>= pushi
ir        op = (op      <$> popi)                                       >>= pushr
pr        op = (op      <$> popp)                                       >>= pushr
rrrp      op = (flip3 op<$> popr <*> popr <*> popr)                     >>= pushp
iib       op = (flip op <$> popi <*> popi)                              >>= pushb
rrb       op = (flip op <$> popr <*> popr)                              >>= pushb
aiv       op = (flip op <$> popi <*> popa)                              >>= push
ai        op = (op      <$> popa)                                       >>= pushi
co        op = (op      <$> popc)                                       >>= pusho
orrro     op = (flip4 op<$> popr <*> popr <*> popr <*> popo)            >>= pusho
oro       op = (flip op <$> popr <*> popo)                              >>= pusho
ppl       op = (flip op <$> popp <*> popp)                              >>= pushl
ppprrl    op = (flip5 op<$> popr <*> popr <*> popp <*> popp <*> popp)   >>= pushl
ooo       op = (flip op <$> popo <*> popo)                              >>= pusho
paoiriisR op = (flip8 op<$> pops <*> popi <*> popi <*> popr <*> popi <*> popo <*> popa <*> popp) >>= pushR

operators :: Map String Operator
operators = fromList [ ( "addi"      ,       iii (+)                    ) -- numbers
                     , ( "addf"      ,       rrr (+)                    )
                     , ( "acos"      ,        rr acos                   )
                     , ( "asin"      ,        rr asin                   )
                     , ( "clampf"    ,        rr clampf                 )
                     , ( "cos"       ,        rr cos                    )
                     , ( "divi"      ,       iii div                    )
                     , ( "divf"      ,       rrr (/)                    )
                     , ( "eqi"       ,       iib (==)                   )
                     , ( "eqf"       ,       rrb (==)                   )
                     , ( "floor"     ,        ri floor                  )
                     , ( "frac"      ,        rr (snd . properFraction) ) -- ???
                     , ( "lessi"     ,       iib (>)                    )
                     , ( "lessf"     ,       rrb (>)                    )
                     , ( "modi"      ,       iii mod                    )
                     , ( "muli"      ,       iii (*)                    )
                     , ( "mulf"      ,       rrr (*)                    )
                     , ( "negi"      ,        ii negate                 )
                     , ( "negf"      ,        rr negate                 )
                     , ( "real"      ,        ir fromIntegral           )
                     , ( "sin"       ,        rr sin                    )
                     , ( "sqrt"      ,        rr sqrt                   )
                     , ( "subi"      ,       iii (-)                    )
                     , ( "subf"      ,       rrr (-)                    )
                     , ( "getx"      ,        pr getX3D                 ) -- points
                     , ( "gety"      ,        pr getY3D                 )
                     , ( "getz"      ,        pr getZ3D                 )
                     , ( "point"     ,      rrrp (\x y z -> Vector3D (x, y, z)))
                     , ( "get"       ,       aiv (!!)                   ) -- arrays
                     , ( "length"    ,        ai length                 )
                     , ( "sphere"    ,        co (Simple Shape.Sphere   . gmlShader)) -- Primitive Objects
                     , ( "cube"      ,        co (Simple Shape.Cube     . gmlShader))
                     , ( "cylinder"  ,        co (Simple Shape.Cylinder . gmlShader))
                     , ( "cone"      ,        co (Simple Shape.Cone     . gmlShader))
                     , ( "plane"     ,        co (Simple Shape.Plane    . gmlShader))
                     , ( "translate" ,     orrro Translate              ) --Transformations
                     , ( "scale"     ,     orrro Scale                  ) 
                     , ( "uscale"    ,       oro UScale                 )
                     , ( "rotatex"   ,       oro RotateX                )
                     , ( "rotatey"   ,       oro RotateY                )
                     , ( "rotatez"   ,       oro RotateZ                )
                     , ( "light"     ,       ppl Light.DirectLight      ) --Lights
                     , ( "pointlight",       ppl Light.PointLight       )
                     , ( "spotlight" ,    ppprrl Light.SpotLight        )
                     , ( "union"     ,       ooo Union                  ) --Boolean operators
                     , ( "intersect" ,       ooo Intersect              )
                     , ( "difference",       ooo Difference             )
                     , ( "render"    , paoiriisR renderF                )
                     ]

--Convert light array types
renderF::Point -> Array -> Object -> Int -> Double -> Int -> Int -> String -> Scene
renderF p a = Scene p (map (\(Light l) -> l) a)

runOp::(String,Operator) -> Stack -> Stack
runOp (nm,op) st = let er e = error ("error running operator "++nm++": "++e++(show st))
                   in either er id (execStateT op st)

clampf :: Double -> Double
clampf r1 | r1 < 0.0  = 0.0
          | r1 > 1.0  = 1.0
          | otherwise = r1

