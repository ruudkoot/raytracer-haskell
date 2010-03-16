module Renderer.UV where

import Base.Shader

import Data.Vector

import Debug.Trace
------------------------------------------------------------------------------

-- uvSphere :: Pt3D -> SurfaceCoord
-- uvSphere loc = let theta = acos  ( getY3D loc )
--                    phi   = atan2 ( getX3D loc ) (getZ3D loc) - pi
--                    pi2   = 2*pi
--                    u     = phi / pi2
--                    v     = pi - theta / pi
--                in trace (show loc  ++ " -> " ++ show u ++ " " ++ show v) 
--                   $ ( 0               -- face
--                   , phi / pi2       -- u
--                   , pi - theta / pi -- v
--                   )

uvSphere :: Pt3D -> SurfaceCoord
uvSphere loc = let y = getY3D loc
                   z = getZ3D loc
                   v = 2*y + 2
                   u = acos (z / sqrt (1 - y*y)) / 360
                in trace (show loc ++ " -> " ++ show (u,v)) $ 
                         (0, u, v)


uvPlane :: Pt3D -> SurfaceCoord
uvPlane loc = (0, getX3D loc, getZ3D loc)
              
uvCube :: Pt3D -> SurfaceCoord
uvCube loc = case fromVector3D loc of
               (u, v, 0) -> (0, u, v) -- front
               (u, v, 1) -> (1, u, v) -- back
               (0, v, u) -> (2, u, v) -- left
               (1, v, u) -> (3, u, v) -- right
               (u, 0, v) -> (4, u, v) -- top
               (u, 1, v) -> (5, u, v) -- bottom 
               _         -> error $ "Loc (" ++ show loc ++ ") is not a valid "
                                    ++ "cube coordinate... What where you "
                                    ++ "thinking?"