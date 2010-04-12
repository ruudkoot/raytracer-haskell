{-# LANGUAGE DeriveDataTypeable #-}
-- | File containing the commandline args datastructure, factored out for usage
--   further in the program.
module Base.CLI where

import System.Console.CmdArgs

data CLIArg = CLIArg 
  { bloom :: Bool
  , aa    :: Int
  , files :: [String]
  } deriving (Show, Data, Typeable)