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
<<<<<<< HEAD:src/Project.hs
main = do gml <- doParseGML          
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])
=======

{-main = do parseResult <- parseGML <$> getContents
          case parseResult of
            Left err -> putStrLn $ "Parse error on " ++ show err
            Right (tks) -> print (evaluate (M.empty,[],tks))
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])-}
>>>>>>> d0f2c78ddbb4377ba0d746a2db04966bb6256472:src/Project.hs

main = Renderer.Renderer.renderTest $ toRenderObject (RotateZ (UScale (Union (Simple Sphere (solid 1 0 0)) (Simple Cone (solid 1 0 0))) 0.2) pi)
