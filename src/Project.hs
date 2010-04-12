module Main where 

import Base.CLI(parseCArgs, CLArgs(..), defaultargs)
import Input.GML.RunGML (runGML, toWorld)
import Renderer.Renderer (renderScene)
--
import System
import System.IO

main :: IO()
main = do args <- fmap (parseCArgs parameters defaultargs) getArgs  
          (scenes, textures) <- runGML $ file args
          mapM_ (renderScene . toWorld textures) scenes


usage :: IO ()
usage = putStrLn $ unlines 
          [ "raytrace 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, The Ray Team" ]

parameters = [
              ("bloom", \(args, inp)  -> (args { bloom = True  }, inp))
             ,("aa"   , \(args, i:xs) -> (args { aa    = read i}, xs ))
             ]