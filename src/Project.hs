module Main where 

import Output.Output
import Output.PPM

main :: IO()
main = maybe (putStrLn "Not a valid image") putStrLn (toPPM (Size 0) (Size 0) [])
