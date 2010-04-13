{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Cone where

import Postlude
import Base.Shape
import Data.Vector

data Cone = Cone
data Face = Base | LateralSurface deriving Enum

instance Shape Cone Face where
    getNormal' _ v | 1.0 ~= y  = vector3D (0.0, 1.0, 0.0)
                   | otherwise = normalize $ vector3D (2*x, -2*y, 2*z)
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    intervals' _ r = let (px, py, pz) = tupleFromVector $ rOrigin r
                         (vx, vy, vz) = tupleFromVector $ rDirection r
                         a            = vx ^ 2 + vz ^ 2 - vy ^ 2
                         b            = 2 * (px * vx + pz * vz - py * vy)
                         c            = px ^ 2 + pz ^ 2 - py ^ 2
                         -- If the side is hit on exactly one point, then there must
                         -- be an intersection with the top too.
                         solveTop s   = case filter (\t -> (py + vy * t) >= 0 && (py + vy * t) <= 1) s of
                                          [x] -> [(1 - py)/vy, x]
                                          []  -> []
                                          ls  -> ls
                      in solveTop $ solveQuadratic a b c
    uv'        _ v | y ~= 1.0  = (LateralSurface, (x + 1)/2, (z + 1)/2)
                   | otherwise = (Base          , acos(z/y)/(2*pi), y)
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
