module Main where 

import Control.Applicative ((<$>))

import Input.GML.AST
import Input.GML.Parser
import Input.GML.Scene
import Base.Shape
import Input.GML.Evaluate
import Input.GML.ToRenderObject
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

test::String
test ="{ /a /b /c b c } /solveRoot 3.0 4.0 5.0 solveRoot apply"

main :: IO()
{-main = do parseResult <- parseGML <$> getContents
          case parseResult of
            Left err -> putStrLn $ "Parse error on " ++ show err
            Right (tks) -> print (evaluate (M.empty,[],tks))
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])-}

main = Renderer.Renderer.renderTest $ toRenderObject (RotateZ (UScale (Union (Simple Sphere (solid 1 0 0)) (Simple Cone (solid 1 0 0))) 0.2) pi)
