{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Input.GML.AST where

import           Control.Monad
import           Data.Char
import qualified Data.Map        as Map
import           Test.QuickCheck
import           Data.Typeable
import           Shared.Vector (Vector3D)
import           Shared.RenderBase
import           Input.GML.Render
--AST defition following the specification in chapter 2.1 of the assignment

-- * Parser
type GML = [Token]

data Token     = Function   [Token]
               | TArray     [Token]
               | Operator   String
               | Identifier String
               | Binder     String
               | TBaseValue BaseValue
               deriving (Show, Eq)
 
data BaseValue = Int      Int
               | Real     Double
               | Boolean  Bool
               | String   String
                deriving (Show, Typeable)

-- * Stack
type Id        = String
type Env       = Map.Map Id Value
type Code      = GML
type Closure   = (Env, Code)

type Point     = Vector3D Double
type Object    = GMLObject
type Light     = RenderLight

data Value     = BaseValue BaseValue
               | Closure   Closure
               | Array     Array
               | Point     Point
               | Object    Object
               | Light     Light
               | Render    GMLRender
               deriving (Show, Eq, Typeable)
               
type Array     = [Value]
type Stack     = [Value]

type State     = (Env, Stack, Code) {- abstract newtype... -}

--Needed for overiding double equality to account for rounding errors in tests
instance Eq BaseValue where
    (==) (Real d1)    (Real d2)      = abs (d1-d2) < 0.00001
    (==) (String s1)  (String s2)    = s1 == s2
    (==) (Boolean b1) (Boolean b2)   = b1 == b2
    (==) (Int i1)     (Int i2)       = i1 == i2

--Algebra for folding GML AST's
type GmlAlgebra tok bv = (([tok]     -> tok, --Function
                           [tok]     -> tok, --Array
                           String    -> tok, --Operator
                           String    -> tok, --Identifier
                           String    -> tok, --Binder
                           bv        -> tok),--BaseValue 

                          (Int       -> bv, --Int
                           Double    -> bv, --Real
                           String    -> bv, --String
                           Bool      -> bv --Boolean                           
                           ))

--GML fold
foldGML::GmlAlgebra tok bv -> GML -> [tok]
foldGML ((func,arr,op,ident,bind,base),(int,real,string,bool)) = map foldToken
    where   foldToken (Function ls)     = func (map foldToken ls)
            foldToken (TArray ls)       = arr (map foldToken ls)        
            foldToken (Operator s)      = op s
            foldToken (Identifier s)    = ident s
            foldToken (Binder s)        = bind s            
            foldToken (TBaseValue v)    = base (foldBase v)

            foldBase  (Int i)           = int i
            foldBase  (Real d)          = real d
            foldBase  (String s)        = string s
            foldBase  (Boolean b)       = bool b    

--Algebra for printing GML, uses a very basic strategy, not pretty!
simplePrintAlg::GmlAlgebra String String
simplePrintAlg = ((func,arr,op,ident,bind,base),(int,real,string,bool))
    where   func ls  = '{':concatMap (' ':) ls++"}\n"
            arr ls   = '[':concatMap (' ':) ls++"]"
            op       = id
            ident    = id
            bind     = ('/':)
            base     = id

            int        = show 
            real       = show
            string s   = '\"':s++"\""
            bool True  = "true"
            bool False = "false"


simplePrintGML::GML -> String
simplePrintGML gml = concatMap (' ':) $ foldGML simplePrintAlg gml

--Arbitrary instances for GML        

instance Arbitrary Token where
    arbitrary = sized $ \n -> frequency [(1,liftM Function (resize (n `div` 4) arbitrary))
                                        ,(1,liftM TArray (resize (n `div` 4) arbitrary))
                                        ,(2,genOperator)
                                        ,(2,liftM Identifier genIdent)
                                        ,(1,liftM Binder genIdent)
                                        ,(3,liftM TBaseValue arbitrary)]

instance Arbitrary BaseValue where
    arbitrary = oneof [liftM Int arbitrary
                      ,liftM Real arbitrary
                      ,liftM Boolean arbitrary
                      ,liftM String genString]

genOperator::Gen Token
genOperator = return (Operator "apply")
 
--Generate a list of chars satisfied by one of the functions
allChars::[Char->Bool]->String
allChars fs = filter (\c -> any (\f -> f c) fs) (map chr [32..126])

genIdent::Gen String
genIdent = do fs <- (oneof.map return.allChars) [isLetter]
              rst <- (listOf.oneof.map return.allChars) [isLetter,isDigit,(=='-'),(=='_')]
              return (fs:rst)

genString::Gen String
genString = (listOf1.oneof.map return.allChars) [(/='"')]
                 

