module Tests.Renderer.Intersections where

-- test (Ray o d) = map (\(x,y) -> (x, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF x d), y, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF y d))) (intersection (Ray o d) Cylinder)


prop_cylinderIntersect :: Ray -> Bool
prop_cylinderIntersect (Ray o d) = map (\(x,y) -> isOnCylinder (o + scaleF x d) && isOnCylinder (o + scaleF y d)) (intersection r Cylinder)
   where isOnCylinder v = abs ((getY4 v) - 1.0) < 0.001 || abs ((sqrt ((getX4 v)*(getX4 v) + (getZ4 v)*(getZ4 v))) - 1.0) < 0.001