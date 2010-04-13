module Main where 

import Base.CLI(ProgramOptions(..))
import Input.GML.RunGML (runGML, toWorld)
import Renderer.Renderer (renderScene)
--
import System.Console.CmdArgs
import System.IO

main :: IO()
main = do cargs <- cmdArgs usage [standard]
          mapM_ (renderFile cargs) (files cargs)
 

renderFile :: ProgramOptions -> FilePath -> IO ()
renderFile cargs fp = do putStrLn $ "Rendering file: " ++ fp 
                         (scenes, textures) <- runGML fp
                         mapM_ (renderScene cargs . toWorld textures) scenes          

usage :: String
usage = unlines 
          [ "raytrace 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, The Ray Team" ]

standard :: Mode ProgramOptions
standard = mode $ ProgramOptions 
            { 
              bloom = def &= text "Enable bloom filter"  & empty False
            , aa    = 1   &= text "Enable anti-aliasing" 
            , files = def &= text "FILE"                 & args
            }

