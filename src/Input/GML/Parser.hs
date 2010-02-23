module Input.GML.Parser (parseGML, parseNumber) where
import Text.ParserCombinators.Parsec.Token
import Data.Ratio
import Control.Monad

import Input.GML.ApplicativeParsec
import Input.GML.AST
import Data.Char

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
   , reservedNames  = ["true","false"]
   , caseSensitive  = False
   }

gmlLexer = makeTokenParser gmlDef

parseGML :: String -> Either ParseError TokenList
parseGML = parse (whiteSpace gmlLexer *> parseTokenList <* eof) ""

parseTokenList :: Parser TokenList
parseTokenList = TokenList <$> many parseTokenGroup 
             <?> "token list" 

parseTokenGroup :: Parser TokenGroup
parseTokenGroup = TokenS        <$> parseToken
              <|> TokenFunction <$> braces gmlLexer parseTokenList
              <|> TokenArray    <$> squares gmlLexer parseTokenList
              <?> "token group"

parseToken :: Parser Token
parseToken =  Number      <$> parseNumber
          <|> parseBoolean
          <|> TokenString <$> parseString
          <|> Binder      <$> (reservedOp gmlLexer "/" *> identifier gmlLexer)
          <|> Identifier  <$> identifier gmlLexer
          <?> "token"

--Unescaped string parser
parseString :: Parser String
parseString = char '\"' *> many (satisfy (\x -> isPrint x && x /= '\"')) <* char '\"' <* whiteSpace gmlLexer

parseBoolean :: Parser Token
parseBoolean =  Boolean True  <$ reserved gmlLexer "true"
            <|> Boolean False <$ reserved gmlLexer "false" 
            <?> "boolean"

parseNumber::Parser NumberVal
parseNumber =  do sign <- option 1 (-1 <$ char '-')
                  num <- naturalOrFloat gmlLexer
                  return (case num of
                            Left i  -> (IntVal . fromInteger) (i*sign)
                            Right d -> DoubleVal (d * fromIntegral sign))
           <?> "number"

