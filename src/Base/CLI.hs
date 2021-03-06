{-# LANGUAGE DeriveDataTypeable #-}

-- | File containing the command line 
-- arguments data type; factored out 
-- for usage further in the program.
-- (Project.hs and Renderer/Renderer.hs)
--
module Base.CLI where

import System.Console.CmdArgs

data ProgramOptions = ProgramOptions
  { bloom :: Bool
  , aa    :: Int
  , files :: [String]
  } deriving (Show, Data, Typeable)



usage :: String
usage = unlines 
          [ "raytrace 1.0 - Yet Another Haskell Ray Tracer",
            "Copyright 2010, Team The Ray Team" ]



-- | Standard command line options. 
--
standard :: Mode ProgramOptions
standard = mode ProgramOptions 
            { 
              bloom = def &= text "Enable bloom filter"  
            , aa    = 1   &= text "Enable anti-aliasing" 
            , files = def &= text "FILES" & args
            }

