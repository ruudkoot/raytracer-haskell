module Main where 

import Input.GML.RunGML
import Renderer.Renderer (renderScene)
--
import System
import System.IO

main :: IO()
main = do args <- getArgs           
          scenes <- runGML $ head args
          mapM_ (renderScene . toWorld) scenes


usage :: IO ()
usage = putStrLn $ unlines 
          [ "raytrace 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, The Ray Team" ]
