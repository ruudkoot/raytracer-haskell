module Input.GML.Evaluate {- export list here -} where

import qualified Data.Map      as Map
import qualified Input.GML.AST as GML

{- need to integrate this with GML.AST -}
data BaseValue = Boolean Bool
               | Int     Int
               | Real    Double
               | String  String
               deriving (Show, Eq)
               
data Value     = BaseValue BaseValue
               | Closure   Closure
               | Array     Array
               | Point     Point
               | Object    Object
               | Light     Light
               deriving (Show, Eq)
               
type Code      = GML.TokenList
type Closure   = (Env, Code)
type Array     = [Value]
type Env       = Map.Map Id Value
type Stack     = [Value]
type Code      = GML.TokenList
type Id        = String                -- only used, not defined, in task descr.

type State = (Env, Stack, Code) {- abstract newtype... -}

evaluate :: State -> State
evaluate (g,                    a, BaseValue i{-type incorrect!-}: c) = (        g, BaseValue i     : a, c)
evaluate (g, v                : a, GML.TokenS (GML.Binder     x) : c) = (set x v g,                   a, c)
evaluate (g,                    a, GML.TokenS (GML.Identifier x) : c) = (        g, get x g         : a, c)
evaluate (g,                    a, GML.TokenFunction c'          : c) = (        g, Closure (g, c') : a, c)
evaluate (g, Closure (g', c') : a, GML.TokenString   "apply"     : c) = let (g'', b, []) = evaluate (g',  a, c')
                                                                         in (g, b, c)
evaluate (g,                    a, GML.TokenArray    c'          : c) = let (g', vs, []) = evaluate (g', [], c')
                                                                         in (g, vs ++ a, c)
evaluate (g, (g2,c2):(g1,c1):BaseValue (Boolean True ):a, GML.TokenString "if" : c) = let (g'', b, []) = evaluate (g1, a, c1)
                                                                         in (g, b, c)
evaluate (g, (g2,c2):(g1,c1):BaseValue (Boolean False):a, GML.TokenString "if" : c) = let (g'', b, []) = evaluate (g2, a, c2)
                                                                         in (g, b, c)
evaluate (g, a : b {-++???-}, OPERATOR : c                          ) = let a' = operator a
                                                                         in (g, a' : b, c)
                                                                         

operator = error "OPERATOR"

set k v m = Map.insert k v m
get k   m = Map.findWithDefault (error "unknown identifier") k m
