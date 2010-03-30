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
              
uv Cube v | 0.0 `dEq` z = (5, x, y) -- front
          | 1.0 `dEq` z = (4, x, y) -- back
          | 0.0 `dEq` x = (3, z, y) -- left
          | 1.0 `dEq` x = (2, z, y) -- right
          | 0.0 `dEq` y = (0, x, z) -- top
          | 1.0 `dEq` y = (1, x, z) -- bottom 
          | otherwise = error $ "Loc (" ++ show v ++ ") is not a valid "
                            ++ "cube coordinate... What where you "
                            ++ "thinking?"
        where x = getX3D v
              y = getY3D v
              z = getZ3D v

uv Cylinder loc = (0,0.0,0.0)

uv Cone loc = (0,0.0,0.0)

dEq::Double -> Double -> Bool
dEq d1 d2 = abs (d1-d2) < 0.001
