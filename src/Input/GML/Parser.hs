module Input.GML.Parser (parseGML) where
import Text.ParserCombinators.Parsec.Token
import Control.Monad

import Input.GML.ApplicativeParsec
import Input.GML.AST
import Data.Char
import qualified Data.Map as Map

import Input.GML.Operators

gmlOperators = ["apply","if"] ++ Map.keys operators

--Definitions for the lexer created by parsec, see parsec documentation 2.8/2.9 and refernce guide
gmlDef::LanguageDef a
gmlDef
 = LanguageDef
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = "%"
   , nestedComments = False
   , identStart     = letter
   , identLetter    = alphaNum <|> char '-' <|> char '_'
   , opStart        = empty
   , opLetter       = empty
   , reservedOpNames= ["/"]
   , reservedNames  = ["true","false"]++gmlOperators
   , caseSensitive  = False
   }

--Parsec lexer
gmlLexer::TokenParser a
gmlLexer = makeTokenParser gmlDef

parseGML :: String -> Either ParseError [Token]
parseGML = parse (whiteSpace gmlLexer *> parseTokenList <* eof) ""

parseTokenList :: Parser [Token]
parseTokenList = many parseToken
             <?> "token list" 

parseToken :: Parser Token
parseToken =  Function   <$> braces gmlLexer parseTokenList
          <|> TArray     <$> squares gmlLexer parseTokenList
          <|> parseOperator   
          <|> Binder     <$> (reservedOp gmlLexer "/" *> identifier gmlLexer)
          <|> TBaseValue <$> parseBaseValue
          <|> Identifier <$> identifier gmlLexer          
          <?> "token"

parseOperator :: Parser Token
parseOperator =  choice (map parseOp gmlOperators)
             <?> "operator"
       where parseOp s = Operator s <$ reserved gmlLexer s

parseBaseValue :: Parser BaseValue
parseBaseValue =  parseNumber
              <|> parseBoolean
              <|> String <$> parseString       
              <?> "base value"


--Unescaped string parser
parseString :: Parser String
parseString = char '\"' *> many (satisfy (\x -> isPrint x && x /= '\"')) <* char '\"' <* whiteSpace gmlLexer

parseBoolean :: Parser BaseValue
parseBoolean =  Boolean True  <$ reserved gmlLexer "true"
            <|> Boolean False <$ reserved gmlLexer "false" 
            <?> "boolean"

--Customized number lexer, parsec's standard number lexer is not signed, so added that
parseNumber::Parser BaseValue
parseNumber =  do sign <- option 1 (-1 <$ char '-')
                  num <- naturalOrFloat gmlLexer
                  return (case num of
                            Left i  -> (Int . fromInteger) (i*sign)
                            Right d -> Real (d * fromIntegral sign))
           <?> "number"

