module Main where 

import Control.Applicative ((<$>))

import Input.GML.Parser
import Input.GML.Scene
import Input.GML.Evaluate
import Input.GML.ToRenderObject
--
import Renderer.Renderer (render)
--
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
                      else readHandle $ head args 
          gml <- doParseGML handler
          threads <- return 2 -- TODO: specify number of threads via command line arguments
          render threads . toWorld $ doEvaluateGML gml      

readHandle :: FilePath -> IO Handle
readHandle = flip openFile ReadMode 


usage :: IO ()
usage = putStrLn $ unlines 
          [ "raytrace 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, The Ray Team" ]
