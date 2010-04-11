{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs      #-}

module Input.GML.AST where

import           Control.Monad

import           Data.Angle
import           Data.Char
import qualified Data.Map        as Map
import           Data.Vector

import qualified Base.Light      as Light
import           Base.Shape

import           Test.QuickCheck

---------------------- Input.GML.Scene -----------------------------------------

data Scene = Scene
  { sceneAmbience :: Pt3D
  , sceneLights   :: [Light.RenderLight]
  , sceneObj      :: Object
  , sceneDepth    :: Int
  , sceneFov      :: Degrees
  , sceneWidth    :: Int
  , sceneHeight   :: Int
  , sceneFile     :: FilePath
  }
  --deriving (Show,Eq)

data Object where
    Simple     :: forall s f. Shape s f => s -> Closure -> Object
    Translate  :: Object -> Double -> Double -> Double  -> Object
    Scale      :: Object -> Double -> Double -> Double  -> Object
    UScale     :: Object -> Double                      -> Object
    RotateX    :: Object -> Degrees                     -> Object
    RotateY    :: Object -> Degrees                     -> Object
    RotateZ    :: Object -> Degrees                     -> Object
    Union      :: Object -> Object                      -> Object
    Intersect  :: Object -> Object                      -> Object
    Difference :: Object -> Object                      -> Object
    --deriving (Show, Eq)

-- | Wrap the transformations for simple shapes into a newtype, so the algebra
-- doesn't need impredicative types.
newtype SimpleTransformer r = SimpleTransformer {
        transformation :: forall s f. Shape s f => s -> Closure -> r
    }

type ObjectAlgebra r =
    ( SimpleTransformer                  r
    , r -> Double -> Double -> Double -> r
    , r -> Double -> Double -> Double -> r
    , r -> Double                     -> r
    , r -> Degrees                    -> r
    , r -> Degrees                    -> r
    , r -> Degrees                    -> r
    , r -> r                          -> r
    , r -> r                          -> r
    , r -> r                          -> r
    )

foldObject :: ObjectAlgebra r -> Object -> r
foldObject (simple, translate, scale, uscale, rotatex, rotatey, rotatez, union,
              intersect, difference) = fold
    where fold x = case x of {
        Simple     shape   shader   -> (transformation simple) shape shader;
        Translate  object  d1 d2 d3 -> translate  (fold object ) d1 d2 d3;
        Scale      object  d1 d2 d3 -> scale      (fold object ) d1 d2 d3;
        UScale     object  double   -> uscale     (fold object ) double;
        RotateX    object  double   -> rotatex    (fold object ) double;
        RotateY    object  double   -> rotatey    (fold object ) double;
        RotateZ    object  double   -> rotatez    (fold object ) double;
        Union      object1 object2  -> union      (fold object1) (fold object2);
        Intersect  object1 object2  -> intersect  (fold object1) (fold object2);
        Difference object1 object2  -> difference (fold object1) (fold object2);
      }
      
---------------------------------[]---------------------------------------------

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
                deriving (Show)

-- * Stack
type Id        = String
type Env       = Map.Map Id Value
type Code      = [Token]
type Closure   = (Env, Code)

type Point     = Vector3D
type Light     = Light.RenderLight

data Value     = BaseValue BaseValue
               | Closure   Closure
               | Array     Array
               | Point     Point
               | Object    Object
               | Light     Light
               | Render    Scene
               --deriving (Show, Eq)

-- deriving non-haskell 98 types is not possible in 6.10 :/
instance Show Value where
    show x = case x of
               BaseValue v -> show v
               Closure (_, c) -> "Closure " ++ show c
               Array a -> show a
               Point p -> show p
               Object _ -> "<<object>>"
               Light l -> show l
               Render _ -> "<<scene>>"              
               
type Array     = [Value]
type Stack     = [Value]

type State     = (Env, Stack, Code) {- abstract newtype... -}

--Needed for overiding double equality to account for rounding errors in tests
instance Eq BaseValue where
    (==) (Real d1)    (Real d2)      = abs (d1-d2) < 0.00001
    (==) (String s1)  (String s2)    = s1 == s2
    (==) (Boolean b1) (Boolean b2)   = b1 == b2
    (==) (Int i1)     (Int i2)       = i1 == i2
    (==) _            _              = False

--Algebra for folding GML AST's
type CodeAlgebra tok bv = (([tok]     -> tok, --Function
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
foldCode:: CodeAlgebra tok bv -> Code -> [tok]
foldCode ((func,arr,op,ident,bind,base),(int,real,string,bool)) = map foldToken
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
simplePrintAlg::CodeAlgebra String String
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


simplePrintGML::Code -> String
simplePrintGML = concatMap (' ':) . foldCode simplePrintAlg

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
                 

