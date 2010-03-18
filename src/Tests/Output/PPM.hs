module Tests.Output.PPM where

import Output.Output
import Output.PPM
import Test.QuickCheck

prop_Euuh = toPPM (Size 10) (Size 10) [] == Nothing
