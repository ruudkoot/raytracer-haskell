module Input.GML.AST where
import Test.QuickCheck
import Data.Char
import Control.Monad

--AST defition following the specification in chapter 2.1 of the assignment
type GML = [Token]

data Token = Function   [Token]
           | Array      [Token]
           | Operator   String
           | Identifier String
           | Binder     String
           | BaseValue  BaseValue
           deriving (Show, Eq)
 
data BaseValue = Int      Int
               | Real     Double
               | Boolean  Bool
               | String   String
                deriving (Show, Eq)

--Needed for overiding double equality to account for rounding errors in tests
instance Eq NumberVal where
    (==) (IntVal i1) (IntVal i2)       = i1 == i2
    (==) (DoubleVal d1) (DoubleVal d2) = abs (d1-d2) < 0.00001
    (==) _ _ = False


--Algebra for folding GML AST's
type GmlAlgebra ls gr tok num = ([gr] -> ls --TokenList

                                ,(tok  -> gr, --TokenS
                                  ls   -> gr, --TokenFunction
                                  ls   -> gr) --TokenArray

                                ,(String    -> tok, --Identifier
                                  String    -> tok, --Binder
                                  Bool      -> tok, --Boolean
                                  num       -> tok, --Number
                                  String    -> tok) --TokenString

                                ,(Int    -> num, --IntVal
                                  Double -> num)) --DoubleVal

--GML fold
foldGML::GmlAlgebra ls gr tok num -> GML -> ls
foldGML (list,(tok,func,arr),(ident,bind,bool,nm,str),(intv,doubv)) = foldList
    where   foldList (TokenList l)      = list (map foldGroup l)
            
            foldGroup (TokenS t)        = tok (foldToken t)
            foldGroup (TokenFunction l) = func (foldList l)
            foldGroup (TokenArray l)    = arr (foldList l)
        
            foldToken (Identifier s)    = ident s
            foldToken (Binder s)        = bind s
            foldToken (Boolean b)       = bool b
            foldToken (Number n)        = nm (foldNumber n)
            foldToken (TokenString s)   = str s

            foldNumber (IntVal i)       = intv i
            foldNumber (DoubleVal d)    = doubv d

--Algebra for printing GML, uses a very basic strategy, not pretty!
simplePrintAlg::GmlAlgebra String String String String
simplePrintAlg = (list,(tok,func,arr),(ident,bind,bool,num,str),(intv,doubv))
    where   list    = concat 

            tok t   = ' ':t++" "
            func l  = '{':l++"}\n"
            arr l   = '[':l++"]"

            ident   = id
            bind    = ('/':)
            bool True  = "true"
            bool False = "false"
            num     = id
            str s   = '\"':(s++"\"")

            intv    = show 
            doubv   = show 

simplePrintGML::GML -> String
simplePrintGML = foldGML simplePrintAlg

--Arbitrary instances for GML        
instance Arbitrary TokenList where
    arbitrary = liftM TokenList (listOf1 arbitrary)

instance Arbitrary TokenGroup where
    arbitrary = sized $ \n -> frequency [(8,liftM TokenS arbitrary)
                                        ,(1,liftM TokenFunction (resize (n `div` 4) arbitrary))
                                        ,(1,liftM TokenArray (resize (n `div` 4) arbitrary))]

instance Arbitrary Token where
    arbitrary = oneof [liftM Identifier genIdent
                      ,liftM Binder genIdent
                      ,liftM Boolean arbitrary
                      ,liftM Number arbitrary
                      ,liftM TokenString genString]

--Generate a list of chars satisfied by one of the functions
allChars::[Char->Bool]->String
allChars fs = filter (\c -> any (\f -> f c) fs) (map chr [32..126])

genIdent::Gen String
genIdent = do fs <- (oneof.map return.allChars) [isLetter]
              rst <- (listOf.oneof.map return.allChars) [isLetter,isDigit,(=='-'),(=='_')]
              return (fs:rst)

genString::Gen String
genString = (listOf1.oneof.map return.allChars) [(/='"')]
                 
instance Arbitrary NumberVal where
    arbitrary = oneof [liftM IntVal arbitrary, liftM DoubleVal arbitrary]
