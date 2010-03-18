module Tests.Renderer.Intersections where

-- test (Ray o d) = map (\(x,y) -> (x, magnitude $ Vector4DD (1, 0, 1, 0) * (o + scaleF x d), y, magnitude $ Vector4DD (1, 0, 1, 0) * (o + scaleF y d))) (intersection (Ray o d) Cylinder)

import Renderer.Intersections
import Data.Vector

prop_cylinderIntersect :: Ray -> Bool
prop_cylinderIntersect (Ray o d) = map (\(x,y) -> isOnCylinder (o + scaleF x d) && isOnCylinder (o + scaleF y d)) (intersection r Cylinder)
   where isOnCylinder v = abs ((getY4D v) - 1.0) < 0.001 || abs ((sqrt ((getX4D v)*(getX4D v) + (getZ4D v)*(getZ4D v))) - 1.0) < 0.001
