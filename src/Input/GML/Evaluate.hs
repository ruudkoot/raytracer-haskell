module Input.GML.Evaluate (evaluate) where

import qualified Data.Map      as Map
import qualified Input.GML.AST
               
evaluate :: State -> State
evaluate = until (null . \(_, _ , x) -> x) evaluate'

evaluate' :: State -> State
evaluate' (g,                    a, BaseValue  i       : c) = (        g, BaseValue i     : a, c)
evaluate' (g, v                : a, Binder     x       : c) = (set x v g,                   a, c)
evaluate' (g,                    a, Identifier x       : c) = (        g, get x g         : a, c)
evaluate' (g,                    a, Function   c'      : c) = (        g, Closure (g, c') : a, c)
evaluate' (g, Closure (g', c') : a, Operator   "apply" : c) = let (g'', b, []) = evaluate (g',  a, c')
                                                               in (g, b, c)
evaluate' (g,                    a, Array      c'      : c) = let (g', vs, []) = evaluate (g', [], c')
                                                               in (g, vs ++ a, c)
evaluate' (g,                    q, Operator   "if"    : c) = let (g2, c2) : (g1, c1) : BaseValue (Boolean True ) : a = q
                                                                  (g'', b, []) = evaluate (g1, a, c1)
                                                               in (g, b, c)
evaluate' (g,                    q, Operator   "if"    : c) = let (g2, c2) : (g1, c1) : BaseValue (Boolean False) : a = q
                                                                  (g'', b, []) = evaluate (g2, a, c2)
                                                               in (g, b, c)
evaluate' (g,                    b, Operator   o       : c) = let (a', b') = operator o b
                                                               in (g, a' : b', c)
                                                             
operator :: String -> Operator
operator o = Map.findwithDefault (error "unknown operator") o operators

set = Map.insert
get = Map.findWithDefault (error "unknown identifier")
