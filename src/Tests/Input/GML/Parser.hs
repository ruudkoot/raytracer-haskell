module Tests.Input.GML.Parser where
import Input.GML.AST
import Input.GML.Parser
import Test.QuickCheck
import Text.ParserCombinators.Parsec

parseResult::Either ParseError GML -> GML
parseResult (Right l) = l
parseResult (Left _) = []

prop_parseOK::GML -> Bool
prop_parseOK gm = (parseResult.parseGML.simplePrintGML) gm == gm  
