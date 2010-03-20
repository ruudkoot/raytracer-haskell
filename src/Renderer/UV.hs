module Renderer.UV where

import Base.Shader

import Data.Vector

------------------------------------------------------------------------------

type UVMapper = Pt3D -> SurfaceCoord

uvSphere :: UVMapper
uvSphere loc = let theta = acos  ( getY3D loc )
                   phi   = (atan2 ( getX3D loc ) (getZ3D loc))
                   pi2   = 2*pi
                   u     = phi / pi2
                   v     = (pi - theta) / pi
               in ( 0, u, v )


uvPlane :: UVMapper
uvPlane loc = (0, getX3D loc, getZ3D loc)
              
uvCube :: UVMapper
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

uvCylinder :: UVMapper 
uvCylinder loc = undefined
