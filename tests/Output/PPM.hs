module PPMTest where

import Hooray.Output.PPM
import Test.QuickCheck

prop_Euuh = quickCheck (toPPM 10 10 [] == Nothing)
