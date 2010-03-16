module Input.GML.Evaluate (evaluate, shader) where

import           Data.Colour
import qualified Data.Map      as Map

import           Base.Shader

import           Input.GML.AST
import           Input.GML.Operators

evaluate :: State -> State
evaluate = until (null . \(_, _ , x) -> x) evaluate'
    where evaluate' :: State -> State
          evaluate' (g,                    a, TBaseValue i       : c) = (        g, BaseValue i     : a, c)
          evaluate' (g, v                : a, Binder     x       : c) = (set x v g,                   a, c)
          evaluate' (g,                    a, Identifier x       : c) = (        g, get x g         : a, c)
          evaluate' (g,                    a, Function   c'      : c) = (        g, Closure (g, c') : a, c)
          evaluate' (g, Closure (g', c') : a, Operator   "apply" : c) = let (g'', b, []) = evaluate (g',  a, c')
                                                                         in (g, b, c)
          evaluate' (g,                    a, TArray      c'     : c) = let (g', vs, []) = evaluate (g, [], c')
                                                                         in (g, Array vs: a, c)
          evaluate' (g, Closure (g2, c2) : Closure (g1, c1) : BaseValue (Boolean True ) : a, Operator   "if"    : c) = let (g'', b, []) = evaluate (g1, a, c1)
                                                                         in (g, b, c)
          evaluate' (g, Closure (g2, c2) : Closure (g1, c1) : BaseValue (Boolean False) : a, Operator   "if"    : c) = let (g'', b, []) = evaluate (g2, a, c2)
                                                                         in (g, b, c)
          evaluate' (g,                    b, Operator   o       : c) = (g, runOp (o, operator o) b, c)
          evaluate' st                                                = error ("Error in evaluation, state dump:\n"++show st)
                                                             
operator :: String -> Operator
operator o = Map.findWithDefault (error ("unknown operator "++o)) o operators

set = Map.insert
get = Map.findWithDefault (error "unknown identifier")



shader :: Closure -> Shader
shader (e, c) = Shader { runShader = \(face, u, v) -> let s                                                                       = [BaseValue (Int face), BaseValue (Real u), BaseValue (Real v)]
                                                          (e', s', c')                                                            = evaluate (e, s, c)
                                                          [Point p, BaseValue (Real kd), BaseValue (Real ks), BaseValue (Real n)] = s'
                                                       in SurfaceProperty { colour                        = fromVector p
                                                                          , diffuseReflectionCoefficient  = kd
                                                                          , specularReflectionCoefficient = ks
                                                                          , phongExponent                 = n
                                                                          }
                       }

