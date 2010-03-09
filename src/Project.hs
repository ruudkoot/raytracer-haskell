module Main where 

import Control.Applicative ((<$>))

import Input.GML.AST
import Input.GML.Parser
import Input.GML.Evaluate
import Input.GML.ToRenderObject
import Input.GML.Scene
--
import Renderer.Datatypes
import Renderer.Intersections
import Renderer.Shaders
import Renderer.Renderer
--
import Output.Output
import Output.PPM

import System.IO

import Data.Map as M

doParseGML::IO GML
doParseGML = do parseResult <- parseGML <$> getContents
                case parseResult of
                  Left err -> error $ "Parse error on " ++ show err
                  Right (tks) -> return tks

doEvaluateGML::GML->Scene
doEvaluateGML gml = case evaluate (M.empty,[],gml) of
                        (_,(Render s:_),_) -> s
                        _            -> error "No render object found"
           
main :: IO()
main = do gml <- doParseGML          
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])

-- main = Renderer.Renderer.renderTest
