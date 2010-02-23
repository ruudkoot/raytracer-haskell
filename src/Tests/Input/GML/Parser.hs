module Tests.Input.GML.Parser where
import Input.GML.AST
import Input.GML.Parser
import Test.QuickCheck
import Text.ParserCombinators.Parsec

parseResult::Either ParseError TokenList -> GML
parseResult (Right l) = l
parseResult (Left e) = TokenList []

prop_parseOK::GML -> Bool
prop_parseOK gm = (parseResult.parseGML.simplePrintGML) gm == gm  
