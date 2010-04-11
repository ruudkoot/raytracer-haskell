{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Sphere where

import Postlude
import Base.Shape
import Data.Vector

newtype Sphere = Sphere ()

instance Shape Sphere () where
    getNormal' _   = normalize
    intervals' _ r = let (k, dir) = (rOrigin r, rDirection r)
                         a        = dot dir
                         b        = 2.0 * (k !.! dir)
                         c        =  (dot k - 1.0)
                      in solveQuadratic a b c
    uv         _ p = let theta = acos (getY3D p)
                         phi   = atan2 (getX3D p) (getZ3D p)
                         pi2   = 2*pi
                         u     = phi / pi2
                         v     = (pi - theta) / pi                        
                     in (0, u, v)

