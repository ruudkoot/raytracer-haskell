module Renderer.UV where

import Base.Shader
import Base.Shape

import Data.Vector

------------------------------------------------------------------------------

uv :: Shape -> Pt3D -> SurfaceCoord
uv Sphere loc = let theta = acos  ( getY3D loc )
                    phi   = (atan2 ( getX3D loc ) (getZ3D loc))
                    pi2   = 2*pi
                    u     = phi / pi2
                    v     = (pi - theta) / pi
                in ( 0, u, v )


uv Plane loc = (0, getX3D loc, getZ3D loc)
              
uv Cube (Vector3D (x,y,z)) | 0.0 `dEq` z = (0, x, y) -- front
                           | 1.0 `dEq` z = (1, x, y) -- back
                           | 0.0 `dEq` x = (2, z, y) -- left
                           | 1.0 `dEq` x = (3, z, y) -- right
                           | 0.0 `dEq` y = (4, x, z) -- top
                           | 1.0 `dEq` y = (5, x, z) -- bottom 
                           | otherwise = error $ "Loc (" ++ show (Vector3D (x,y,z)) ++ ") is not a valid "
                                              ++ "cube coordinate... What where you "
                                              ++ "thinking?"
 
uv Cylinder loc = (0,0.0,0.0)

uv Cone loc = (0,0.0,0.0)

dEq::Double -> Double -> Bool
dEq d1 d2 = abs (d1-d2) < 0.001
