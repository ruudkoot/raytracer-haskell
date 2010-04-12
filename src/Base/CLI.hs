module Base.CLI where

data CLArgs = CLArgs 
  { aa    :: Int
  , bloom :: Bool
  , file  :: String
  } deriving Show
  
defaultargs = CLArgs { aa = 1,bloom = False, file = "" }

-- | This looks an awful lot like what we used to do in T&C
parseCargs :: [(String, (CLArgs, [String]) -> (CLArgs, [String]) )] -> CLArgs -> [String]  -> CLArgs
parseCargs funcs def input = p (def, input)
  where p (res , [])                = res
        p (res , ('-':flag) : xs) = p (sLookup flag res xs)
        p (res , xs)              = res { file = concat xs }
        sLookup key def inp = maybe (def, inp) 
                                    (\f -> f (def, inp)) 
                                    (lookup key funcs)

-- | Example usage of parseVargs.
ex = parseCargs [
                 ("bloom", \(args, inp)  -> (args { bloom = True  }, inp))
                ,("aa"   , \(args, i:xs) -> (args { aa    = read i}, xs ))
                ]
                defaultargs