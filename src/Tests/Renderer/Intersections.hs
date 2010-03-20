module Tests.Renderer.Intersections where

import Test.QuickCheck
import Renderer.Intersections2
import Renderer.Scene
import Data.Vector
import Data.Matrix
import Base.Shape
import Control.Monad

-- test (Ray o d) = map (\(x,y) -> (x, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF x d), y, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF y d))) (intersection (Ray o d) Cylinder)

margin :: Double
margin = 0.0001

instance Arbitrary Ray where
--  Hitting the unit sphere from a random point in a random direction is rather
--  unlikely, so, to properly test spheres, use:
-- arbitrary = liftM (Ray (Vector4D (0, 0, -4, 0))) arbitrary
--  On the other hand, rays starting from the y = 0-plane, will never hit our
--  unit plane, so for planes use:
  arbitrary = liftM2 Ray arbitrary arbitrary

prop_cylinderIntersect :: Ray -> Property
prop_cylinderIntersect (Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (\(x,y) -> isOnCylinder (mkPoint x) && isOnCylinder (mkPoint y)) intersections
   where isOnCylinder v = isOnTop v || isOnBottom v || isOnSide v
         isOnTop v      = abs (getY3D v) - 1.0 < margin
         isOnBottom v   = abs (getY3D v) < margin
         isOnSide v     = abs (sqrt (getX3D v * getX3D v + getZ3D v * getZ3D v) - 1.0) < margin
         mkPoint t      = dropW (o + scaleF t d)
         intersections  = intervals (Ray o d) Cylinder
         
prop_sphereIntersect :: Ray -> Property
prop_sphereIntersect (Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (\(x,y) -> isOnSphere (mkPoint x) && isOnSphere (mkPoint y)) intersections
  where isOnSphere v   = abs (magnitudeSquared v - 1.0) < margin
        mkPoint t      = dropW (o + scaleF t d)
        intersections  = intervals (Ray o d) Sphere
        
prop_planeIntersect :: Ray -> Property
prop_planeIntersect (Ray o d) = (not.null) intersections && magnitudeSquared d > 0 ==> all (\(x,y) -> isOnPlane (mkPoint x) && isOnPlane (mkPoint y)) intersections
  where isOnPlane v    = abs (getY3D v) < margin
        mkPoint t      = dropW (o + scaleF t d)
        intersections  = intervals (Ray o d) Plane
