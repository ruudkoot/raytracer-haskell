module Main where 

import Control.Applicative ((<$>))

import Input.GML.AST
import Input.GML.Parser
import Input.GML.Evaluator
--
import Renderer.Datatypes
import Renderer.Intersections
--
import Output.Output
import Output.PPM

import System.IO

import Data.Map as M

test::String
test ="{ /a /b /c b c } /solveRoot 3.0 4.0 5.0 solveRoot apply"

main :: IO()
main = do --parseResult <- parseGML <$> getContents
          case parseGML test of 
            Left err -> putStrLn $ "Parse error on " ++ show err
            Right (TokenList tks) -> print (evalGML (M.empty,[],tks))
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])
