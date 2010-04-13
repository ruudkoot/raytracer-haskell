{-# LANGUAGE MultiParamTypeClasses #-}

module Base.Shape.Cube where

import Prelude     hiding (Left, Right)
import Postlude
import Base.Shape
import Data.Vector

data Cube = Cube
data Face = Front | Back | Left | Right | Top | Bottom deriving Enum

instance Shape Cube Face where
    getNormal' _ v | z ~= 0.0  = vector3D ( 0.0, 0.0,-1.0) -- Front
                   | z ~= 1.0  = vector3D ( 0.0, 0.0, 1.0) -- Back
                   | x ~= 0.0  = vector3D (-1.0, 0.0, 0.0) -- Left
                   | x ~= 1.0  = vector3D ( 1.0, 0.0, 0.0) -- Right
                   | y ~= 0.0  = vector3D ( 0.0,-1.0, 0.0) -- Top
                   | y ~= 1.0  = vector3D ( 0.0, 1.0, 0.0) -- Bottom 
                   | otherwise = error "the impossible happened"
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    intervals' _ r =  let (ox,oy,oz)     = tupleFromVector $ rOrigin r
                          (dx,dy,dz)     = tupleFromVector $ rDirection r
                          calcMinMax o d = let dv = 1.0/d
                                               t1  = -o*dv
                                               t2  = (1.0-o)*dv
                                            in if d >= 0.0
                                                 then (t1,t2)
                                                 else (t2,t1)
                          (txl,txh)      = calcMinMax ox dx
                          (tyl,tyh)      = calcMinMax oy dy
                          (tzl,tzh)      = calcMinMax oz dz
                          tmin           = max txl $ max tyl tzl
                          tmax           = min txh $ min tyh tzh
                       in if tmin < tmax then [tmin,tmax] else []
    uv'        _ v | z ~= 0.0  = (Front , x, y)
                   | z ~= 1.0  = (Back  , x, y)
                   | x ~= 0.0  = (Left  , z, y)
                   | x ~= 1.0  = (Right , z, y)
                   | y ~= 0.0  = (Top   , x, z)
                   | y ~= 1.0  = (Bottom, x, z)
                   | otherwise = error "the impossible happened"
                   where x = getX3D v
                         y = getY3D v
                         z = getZ3D v
    boundingBox _ = Bbox (toVec3D 0.0 0.0 0.0) (toVec3D 1.0 1.0 1.0)

