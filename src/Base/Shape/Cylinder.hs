{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Cylinder where

import Postlude
import Base.Shape
import Data.Vector

data Cylinder = Cylinder
data Face     = Outside | Top | Bottom deriving Enum

instance Shape Cylinder Face where
    getNormal' _ v | 0.0 ~= y = vector3D (0.0,-1.0,0.0)
                   | 1.0 ~= y = vector3D (0.0, 1.0,0.0)
                   | otherwise   = normalize $ vector3D (x,0.0,z)
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    intervals' _ r =  let (px, py, pz) = tupleFromVector $ rOrigin r
                          (vx, vy, vz) = tupleFromVector $ rDirection r
                          a            = vx ^ 2 + vz ^ 2
                          b            = 2 * (px * vx + pz * vz)
                          c            = px ^ 2 + pz ^ 2 - 1.0
                          topAndBottom = filter (\t -> let rv = (px + vx * t)^2 + (pz + vz * t)^2 in 0 <= rv && rv <= 1) [- py / vy, (1 - py) / vy]
                       in topAndBottom ++ filter (\t -> let y = py + vy * t in 0 <= y && y <= 1) (solveQuadratic a b c)
    uv'        _ v | y ~= 1.0  = (Top    , (x + 1)/2, (z + 1)/2)
                   | y ~= 0.0  = (Bottom , (x + 1)/2, (z + 1)/2)
                   | otherwise = (Outside, acos z/(2*pi), y)
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    boundingBox _ = Bbox (toVec3D (-1) 0 (-1)) (toVec3D 1 1 1)
