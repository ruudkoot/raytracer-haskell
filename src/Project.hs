module Main where 

import Input.GML.AST
import Input.GML.Parser
--
import Renderer.Datatypes
--
import Output.Output
import Output.PPM

main :: IO()
main = maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])
