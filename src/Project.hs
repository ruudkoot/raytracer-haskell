module Main where 

import Base.CLI(CLIArg(..))
import Input.GML.RunGML (runGML, toWorld)
import Renderer.Renderer (renderScene)
--
import System.Console.CmdArgs
import System.IO

main :: IO()
main = do cargs <- cmdArgs usage [standard]
          mapM_ (\ file -> do putStrLn $ "Rendering file: " ++ file
                              (scenes, textures) <- runGML file
                              mapM_ (renderScene cargs . toWorld textures) 
                                    scenes
                )
                (files cargs)
          

usage :: String
usage = unlines 
          [ "raytrace 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, The Ray Team" ]

standard :: Mode CLIArg
standard = mode $ CLIArg 
            { 
              bloom = def &= text "Enable bloom filter"  & empty "False"
            , aa    = def &= text "Enable anti-aliasing" & empty "1"
            , files = def &= text "FILE"                 & args
            }

