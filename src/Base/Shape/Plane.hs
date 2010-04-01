{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Plane where

import Postlude
import Base.Shape
import Data.Vector

newtype Plane = Plane ()

instance Shape Plane () where
    getNormal' _ _ = vector3D (0.0,1.0,0.0)
    inside     _ p = getY3D p < 0.0
    intervals' _ r = let (oy, ry) = (getY3D $ rOrigin r, getY3D $ rDirection r)
                      in if (oy == 0) || (oy * ry >= 0)
                         then []
                         else [- oy / ry]
    uv         _ v = (0, getX3D v, getZ3D v)

