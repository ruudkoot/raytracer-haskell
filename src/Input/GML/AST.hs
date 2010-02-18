module Input.GML.AST where
import Test.QuickCheck
import Data.Char
import Control.Monad

newtype TokenList = TokenList [TokenGroup] deriving (Show,Eq)

data TokenGroup = TokenS        Token 
                | TokenFunction TokenList
                | TokenArray    TokenList
                deriving (Show,Eq)
                
data Token = Identifier  String
           | Binder      String
           | Boolean     Bool
           | Number      NumberVal
           | TokenString String
            deriving (Show,Eq)

data NumberVal = IntVal    Int
               | DoubleVal Double
                deriving (Show,Eq)

--Arbitrary instances
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
allChars::[(Char->Bool)]->[Char]
allChars fs = filter (\c -> (or.map (\f -> f c)) fs) (map chr [32..255])

genIdent::Gen String
genIdent = (listOf1.oneof.map return.allChars) [isLetter,isDigit,(=='-'),(=='_')]

genString::Gen String
genString = (listOf1.oneof.map return.allChars) [(/='"')]
                 
instance Arbitrary NumberVal where
    arbitrary = oneof [liftM IntVal arbitrary, liftM DoubleVal arbitrary]
