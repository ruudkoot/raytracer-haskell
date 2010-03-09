module Main where 

import Control.Applicative ((<$>))

import Input.GML.AST
import Input.GML.Parser
import Input.GML.Scene
import Base.Shape
import Input.GML.Evaluate
import Input.GML.ToRenderObject
--
import Renderer.Intersections
import Renderer.Shaders
import Renderer.Renderer
import Renderer.Scene
--
import Output.PPM

import System
import System.IO

import qualified Data.Map as M

doParseGML :: Handle -> IO GML
doParseGML h = do parseResult <- parseGML <$> hGetContents h
                  case parseResult of
                    Left  err -> error $ "Parse error on " ++ show err
                    Right tks -> return tks

doEvaluateGML::GML->Scene
doEvaluateGML gml = case evaluate (M.empty,[],gml) of
                        (_, Render s:_, _) -> s
                        _                  -> error "No render object found"
           
main :: IO()
main = do args <- getArgs 
          handler <- if null args 
                      then return stdin
                      else readHandler $ head args 
          gml <- doParseGML handler
          renderScene . toWorld $ doEvaluateGML gml      

readHandler :: FilePath -> IO Handle
readHandler = flip openFile ReadMode 


usage :: IO ()
usage = putStrLn $ unlines 
          [ "raytracer 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, The Ray Team" ]

{-main = do parseResult <- parseGML <$> getContents
          case parseResult of
            Left err -> putStrLn $ "Parse error on " ++ show err
            Right (tks) -> print (evaluate (M.empty,[],tks))
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])-}

--main = Renderer.Renderer.renderTest $ toRenderObject (RotateZ (UScale (Union (Simple Sphere (solid 1 0 0)) (Simple Cone (solid 1 0 0))) 0.2) pi)
