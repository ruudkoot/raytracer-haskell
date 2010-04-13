module GML.Evaluate (evaluate, shader) where

import           Data.Texture
import           Data.Colour
import qualified Data.Map      as Map

import           Base.Shader

import           GML.AST
import           GML.Operators

evaluate :: State -> State
evaluate = until (null . \(_, _ , x) -> x) evaluate'
    where evaluate' :: State -> State
          evaluate' (g,                    a, TBaseValue i       : c) = (        g, BaseValue i     : a, c)
          evaluate' (g, v                : a, Binder     x       : c) = (set x v g,                   a, c)
          evaluate' (g,                    a, Identifier x       : c) = (        g, get x g         : a, c)
          evaluate' (g,                    a, Function   c'      : c) = (        g, Closure (g, c') : a, c)
          evaluate' (g, Closure (g', c') : a, Operator   "apply" : c) = let (_, b, []) = evaluate (g',  a, c')
                                                                         in (g, b, c)
          evaluate' (g,                    a, TArray      c'     : c) = let (_, vs, []) = evaluate (g, [], c')
                                                                         in (g, Array vs: a, c)
          evaluate' (g, Closure _ : Closure (g1, c1) : BaseValue (Boolean True ) : a, Operator   "if"    : c) = let (_, b, []) = evaluate (g1, a, c1)
                                                                         in (g, b, c)
          evaluate' (g, Closure (g2, c2) : Closure _ : BaseValue (Boolean False) : a, Operator   "if"    : c) = let (_, b, []) = evaluate (g2, a, c2)
                                                                         in (g, b, c)
          evaluate' (g,                    b, Operator   o       : c) = (g, runOp (o, operator o) b, c)
          evaluate' st                                                = error ("Error in evaluation, state dump:\n"++show st)
                                                             
operator :: String -> Operator
operator o = Map.findWithDefault (error ("unknown operator: " ++ o)) o operators

set::(Ord k)=>k -> a -> Map.Map k a -> Map.Map k a
set = Map.insert

get::String -> Map.Map String a -> a
get x = Map.findWithDefault (error ("unknown identifier: " ++ x)) x

shader :: Textures -> Closure -> Shader
shader txs (e, c) = Shader { runShader = \(face, u, v) -> let s          = [BaseValue (Real v),BaseValue (Real u),BaseValue (Int face)]
                                                              (_, s', _) = evaluate (e, s,c)                                                          
                                                          in case s' of
                                                              [BaseValue (Real n), BaseValue (Real ks), BaseValue (Real kd), Point p] ->
                                                                SurfaceProperty { surfaceColour                 = toColour p
                                                                                , diffuseReflectionCoefficient  = kd
                                                                                , specularReflectionCoefficient = ks
                                                                                , phongExponent                 = n
                                                                                }
                                                              [BaseValue (Real n), BaseValue (Real ks), BaseValue (Real kd), BaseValue (String tx)] ->
                                                                SurfaceProperty { surfaceColour                 = getLinear' (txs Map.! tx) (u,v)
                                                                                , diffuseReflectionCoefficient  = kd
                                                                                , specularReflectionCoefficient = ks
                                                                                , phongExponent                 = n
                                                                                }                
                                                              _ -> error "Illegal pattern in return value of shader"
                       }

