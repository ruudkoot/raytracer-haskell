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
margin = 0.0001

instance Arbitrary Ray where
  arbitrary = liftM (Ray (Vector4D (0, 0, -4, 0))) arbitrary

prop_cylinderIntersect :: Ray -> Property
prop_cylinderIntersect (Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (\(x,y) -> isOnCylinder (mkPoint x) && isOnCylinder (mkPoint y)) intersections
   where isOnCylinder v = isOnTop v || isOnBottom v || isOnSide v
         isOnTop v      = abs (getY3D v) - 1.0 < margin
         isOnBottom v   = abs (getY3D v) < margin
         isOnSide v     = abs (sqrt ((getX3D v)*(getX3D v) + (getZ3D v)*(getZ3D v)) - 1.0) < margin
         mkPoint t      = dropW (o + scaleF t d)
         intersections  = intervals (Ray o d) Cylinder
         
prop_sphereIntersect :: Ray -> Property
prop_sphereIntersect (Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (\(x,y) -> isOnSphere (mkPoint x) && isOnSphere (mkPoint y)) intersections
  where isOnSphere v   = abs ((magnitudeSquared v) - 1.0) < margin
        mkPoint t      = dropW (o + scaleF t d)
        intersections  = trace (show $ map (\(x,y) -> (magnitudeSquared $ mkPoint x, magnitudeSquared $ mkPoint y)) $ intervals (Ray o d) Sphere) intervals (Ray o d) Sphere