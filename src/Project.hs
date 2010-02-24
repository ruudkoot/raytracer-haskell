module Main where 

import Control.Applicative ((<$>))

import Input.GML.AST
import Input.GML.Parser
--
import Renderer.Datatypes
--
import Output.Output
import Output.PPM


import System.IO


main :: IO()
main = do parseResult <- parseGML <$> getContents
          case parseResult of 
            Left err -> putStrLn $ "Parse error on " ++ show err
            Right tks -> return () -- stub. should evaluate tks
          maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])
