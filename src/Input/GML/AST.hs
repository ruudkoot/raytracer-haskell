module Input.GML.AST where

newtype TokenList = TokenList [TokenGroup]

data TokenGroup = TokenS        Token 
                | TokenFunction TokenList
                | TokenArray    TokenList
                
data Token = Identifier  String
           | Binder      String
           | Boolean     Bool
           | Number      NumberVal
           | TokenString String

data NumberVal = IntVal    Int
               | DoubleVal Double