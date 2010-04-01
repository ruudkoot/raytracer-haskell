{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Cylinder where

import Postlude
import Base.Shape
import Data.Vector

newtype Cylinder = Cylinder ()
data    Face     = Outside | Top | Bottom deriving Enum

instance Shape Cylinder Face where
    getNormal' _        v | 0.0 ~= y = vector3D (0.0,-1.0,0.0)
                          | 1.0 ~= y = vector3D (0.0, 1.0,0.0)
                          | otherwise   = normalize $ vector3D (2*x,0.0,2*z)
                          where x = getX3D v
                                y = getY3D v
                                z = getZ3D v
    inside _        loc = let (x,y,z) = fromVector3D loc
                           in y < 1.0 && y > 0.0 && x*x+z*z < 0.0
    intervals' r _        = 
      let (px, py, pz) = fromVector3D $ rOrigin r
          (vx, vy, vz) = fromVector3D $ rDirection r
          a = vx ^ 2 + vz ^ 2
          b = 2 * (px * vx + pz * vz)
          c = px ^ 2 + pz ^ 2 - 1.0
          topAndBottom = filter (\t -> let r = (px + vx * t)^2 + (pz + vz * t)^2 in 0 <= r && r <= 1) [- py / vy, (1 - py) / vy]
      in topAndBottom ++ (filter (\t -> let y = py + vy * t in 0 <= y && y <= 1) $ solveQuadratic a b c )
    uv           _ v | y ~= 1.0  = (1, (x + 1)/2, (z + 1)/2)
                     | y ~= 0.0  = (2, (x + 1)/2, (z + 1)/2)
                     | otherwise = (0, acos(z)/(2*pi), y)
                     where x = getX3D v
                           y = getY3D v
                           z = getZ3D v

-- | Calculate intersection of a ray and an
-- cylinder of height one.
--
-- 
-- Ray: p + vt
-- Cylinder: x^2 + z^2 = r^2
--
--
--     (px + vx * t)^2 + (pz + vz * t)^2 = r^2 
--   => (radius is 1)
--     (px + vx * t)^2 + (pz + vz * t)^2 - 1 = 0
--   => (expand)
--     (px^2 + (vx*t)^2 + 2*(px*vx*t)) + (pz^2 + (vz*t)^2 + 2*(pz*vz*t) - 1 = 0
--   => (regroup)
--     (px^2 + pz^2 + (vx * t)^2 + (vz*t)^2 + 2*(px*vx*t + pz*vz*t) - 1 = 0
--   => (regroup)
--     (vx^2 + vz^2)*t^2 + 2*(px*vx + pz*vz)*t + px^2 + pz^2 - 1 = 0
--
--
-- Solve with quadratic equation, where
--   a = (vx^2 + vz^2) 
--   b = 2*(px*vx + pz*vz)
--   c = px^2 + pz^2 - 1
--
-- Good source: http://mrl.nyu.edu/~dzorin/intro-graphics/lectures/lecture11/sld002.htm
--
