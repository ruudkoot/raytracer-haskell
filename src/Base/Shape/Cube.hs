{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Cube where

import Postlude
import Base.Shape
import Data.Vector

newtype Cube = Cube ()
data    Face = Front | Back | Top | Bottom | Left | Right deriving Enum

instance Shape Cube Face where
    getNormal' _ v | 0.0 ~= z  = vector3D ( 0.0, 0.0,-1.0) -- front
                   | 1.0 ~= z  = vector3D ( 0.0, 0.0, 1.0) -- back
                   | 0.0 ~= x  = vector3D (-1.0, 0.0, 0.0) -- left
                   | 1.0 ~= x  = vector3D ( 1.0, 0.0, 0.0) -- right
                   | 0.0 ~= y  = vector3D ( 0.0,-1.0, 0.0) -- top
                   | 1.0 ~= y  = vector3D ( 0.0, 1.0, 0.0) -- bottom 
                   | otherwise = error "the impossible happened"
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    inside     _  p = let (x,y,z) = fromVector3D p
                       in x > 0.0 && x < 1.0 && y > 0.0 && y < 1.0 && z > 0.0 && z < 1.0
    intervals' _ r =  let (ox,oy,oz)     = fromVector3D $ rOrigin r
                          (dx,dy,dz)     = fromVector3D $ rDirection r
                          calcMinMax o d = let div = 1.0/d
                                               t1  = -o*div
                                               t2  = (1.0-o)*div
                                            in if d >= 0.0
                                                 then (t1,t2)
                                                 else (t2,t1)
                          (txl,txh)      = calcMinMax ox dx
                          (tyl,tyh)      = calcMinMax oy dy
                          (tzl,tzh)      = calcMinMax oz dz
                          tmin           = max txl $ max tyl tzl
                          tmax           = min txh $ min tyh tzh
                       in if tmin < tmax then [tmin,tmax] else []
    uv         _ v | z ~= 0.0  = (5, x, y) -- front
                   | z ~= 1.0  = (4, x, y) -- back
                   | x ~= 0.0  = (3, z, y) -- left
                   | x ~= 1.0  = (2, z, y) -- right
                   | y ~= 0.0  = (0, x, z) -- top
                   | y ~= 1.0  = (1, x, z) -- bottom 
                   | otherwise = error "the impossible happened"
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v

