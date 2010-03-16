module Tests.Data.Vector where 

import Test.QuickCheck


-- All test function should start with prop_

prop_UnitLength = magnitude unitVector3DX == 1.0
