{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Plane where

import Base.Shape
import Data.Vector
import Postlude

import Base.BoundingSphere

data Plane = Plane

instance Shape Plane () where
    getNormal' _ _ = vector3D (0.0,1.0,0.0)
    intervals' _ r = let (oy, ry) = (getY3D $ rOrigin r, getY3D $ rDirection r)
                      in if oy == 0
                         then []
                         else --[-oy/ry, positiveInfinity]
                              let t = -oy/ry
                              in if oy < 0.0 --aan normal kant ?
                                 then if t > 0.0 --naar plane toe?
                                      then [negativeInfinity, t]
                                      else [t, positiveInfinity]
                                 else if t > 0.0
                                      then [t, positiveInfinity]
                                      else [negativeInfinity ,t]
    uv'        _ v = ((), getX3D v, getZ3D v)
    boundingBox _ = Bbox (toVec3D negativeInfinity 0 negativeInfinity) (toVec3D positiveInfinity 0 positiveInfinity)
    boundingSphere _ = BSphere (toVec3D 0.0 0.0 0.0) positiveInfinity
