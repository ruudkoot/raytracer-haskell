{-# LANGUAGE TypeSynonymInstances #-}
module Tests.Data.Vector where 

import Test.QuickCheck
import Data.Glome.Vec hiding (Ray)
import Control.Monad
import Data.Vector

instance Arbitrary Vec where
    arbitrary = liftM3 toVec3D arbitrary arbitrary arbitrary

instance Arbitrary Ray where
--  Hitting the unit sphere from a random point in a random direction is rather
--  unlikely, so, to properly test spheres, use:
-- arbitrary = liftM (Ray (Vector4D (0, 0, -4, 0))) arbitrary
--  On the other hand, rays starting from the y = 0-plane, will never hit our
--  unit plane, so for planes use:
  arbitrary = liftM2 mkRay arbitrary arbitrary
