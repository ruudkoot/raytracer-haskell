module Tests.Renderer.Intersections where

import Test.QuickCheck
import Renderer.Intersections2
import Renderer.Scene
import Data.Vector
import Data.Matrix
import Base.Shape
import Control.Monad
import Debug.Trace

-- test (Ray o d) = map (\(x,y) -> (x, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF x d), y, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF y d))) (intersection (Ray o d) Cylinder)

margin :: Double
margin = 0.001

instance Arbitrary Ray where
  arbitrary = liftM (Ray (Vector4D (0, 0, -4, 0))) arbitrary

prop_cylinderIntersect :: Ray -> Bool
prop_cylinderIntersect (Ray o d) = magnitudeSquared d == 0 || all (\(x,y) -> isOnCylinder (mkPoint x) && isOnCylinder (mkPoint y)) (intervals (Ray o d) Cylinder)
   where isOnCylinder v = isOnTop v || isOnBottom v || isOnSide v
         isOnTop v      = abs (getY4D v) - 1.0 < margin
         isOnBottom v   = abs (getY4D v) < margin
         isOnSide v     = abs (sqrt ((getX4D v)*(getX4D v) + (getZ4D v)*(getZ4D v)) - 1.0) < margin
         mkPoint t      = o + scaleF t d
         
prop_sphereIntersect :: Ray -> Bool
prop_sphereIntersect (Ray o d) = magnitudeSquared d == 0 || all (\(x,y) -> isOnSphere (mkPoint x) && isOnSphere (mkPoint y)) (intervals (Ray o d) Sphere)
  where isOnSphere v   = abs ((magnitudeSquared $ dropW v) - 1.0) < margin
        mkPoint t      = o + scaleF t d
