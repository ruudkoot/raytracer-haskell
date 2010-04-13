{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Plane where

import Base.Shape
import Data.Vector
import Postlude

newtype Plane = Plane ()

instance Shape Plane () where
    getNormal' _ _ = vector3D (0.0,1.0,0.0)
    intervals' _ r = let (oy, ry) = (getY3D $ rOrigin r, getY3D $ rDirection r)
                      in if (oy == 0)
                         then []
                         else let t = -oy/ry
                              in if oy > 0.0 --aan normal kant ?
                                 then if t > 0.0 --naar plane toe?
                                      then [-positiveInfinity,t]
                                      else [t, positiveInfinity]
                                 else if t > 0.0
                                      then [t, positiveInfinity]
                                      else [-positiveInfinity,t]
    uv         _ v = (0, getX3D v, getZ3D v)

