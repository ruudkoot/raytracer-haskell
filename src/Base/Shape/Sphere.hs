{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Sphere where

import Postlude
import Base.Shape
import Data.Vector

newtype Sphere = Sphere ()

instance Shape Sphere () where
    getNormal'   _ loc = normalize loc
    inside _        loc = magnitude loc < 1.0
    intervals' r _      =
      let (k, dir) = (rOrigin r, rDirection r)
          a = dot dir
          b = 2.0 * (k !.! dir)
          c =  (dot k - 1.0)
      in solveQuadratic a b c
    uv           _ loc   = let theta = acos (getY3D loc)
                               phi   = atan2 (getX3D loc) (getZ3D loc)
                               pi2   = 2*pi
                               u     = phi / pi2
                               v     = (pi - theta) / pi
                          in (0, u, v)

