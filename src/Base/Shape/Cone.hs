{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Cone where

import Postlude
import Base.Shape
import Data.Vector

newtype Cone = Cone ()
data    Face = Base | LateralSurface deriving Enum

instance Shape Cone Face where
    getNormal' _ v | 1.0 ~= y  = vector3D (0.0,1.0,0.0)
                   | otherwise = normalize $ vector3D (2*x,-2*y,2*z)
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    inside     _ p = let (x,y,z) = fromVector3D p
                      in y < 1.0 && y > 0.0 && x*x+z*z-y*y < 0.0
    intervals' _ r = let (px, py, pz) = fromVector3D $ rOrigin r
                         (vx, vy, vz) = fromVector3D $ rDirection r
                         a            = vx ^ 2 + vz ^ 2 - vy ^ 2
                         b            = 2 * (px * vx + pz * vz - (py) * vy)
                         c            = px ^ 2 + pz ^ 2 - py ^ 2
                         -- If the side is hit on exactly one point, then there must
                         -- be an intersection with the top too.
                         solveTop s   = case (filter (\t -> (py + vy * t) >= 0 && (py + vy * t) <= 1)) $ s of
                                          [x] -> [(1 - py)/vy, x]
                                          []  -> []
                                          _    -> s
                      in solveTop $ solveQuadratic a b c
    uv         _ v | y ~= 1.0  = (1, (x + 1)/2, (z + 1)/2)
                   | otherwise = (0, acos(z/y)/(2*pi), y)
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v

{-

| Intersection of a ray with a cone of 
ratio 1 and height 1.

Ray: p + vt
Cone: x^2 + z^2 = (r^2/h^2) * (y - h)^2

   (px + vx * t) ^ 2 + (pz + vz * t) ^ 2 = (r^2/h^2) * (py + vy * t)^2
 => (ratio and height is 1)
   (px + vx * t) ^ 2 + (pz + vz * t) ^ 2 = (py + vy * t)^2
 => (expand)
   (px^2 + (vx*t)^2 + 2*(px*vx*t)) + (pz^2 + (vz*t)^2 + 2*(pz*vz*t)) 
      = py^2 + (vy*t)^2 + 2*(py*vy*t) - 2*py - 2*(vy*t) + 1
 => (regroup)
   px^2 + (vx*t)^2 + 2*(px*vx+pz*vz)*t + pz^2 + (vz*t)^2 - 
      py^2 - (vy*t)^2 - 2*(py*vy*t) + 2*py + 2*(vy*t) - 1 = 0
 => (regroup)
   ((vx*t)^2 + (vz*t)^2 - (vy*t)^2) + 2*(px*vx+pz*vz-py*vy-vy)*t + (px^2 + pz^2 - py^2 + 2*py - 1)


Solve with quadratic equation, where
  a = vx^2 + vz^2 - vy^2
  b = 2*(px*vx + pz*vz - (py + 1) * vy)
  c = px^2 + pz^2 - py^2 + 2*py - 1

-}
