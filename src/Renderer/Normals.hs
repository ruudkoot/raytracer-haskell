module Renderer.Normals where

import Base.Shape
import Data.Vector
import Renderer.Scene (Ray(..))

-- TODO: Incorporate the ray

getNormal :: Shape -> Ray -> Pt3D -> Vec3D 

getNormal Sphere   ray loc = normalize loc

getNormal Plane    ray loc = Vector3D (0.0,1.0,0.0)

getNormal Cylinder ray loc = case fromVector3D loc of
    (_, 0, _) -> Vector3D (0.0,-1.0,0.0)
    (_, 1, _) -> Vector3D (0.0, 1.0,0.0)
    (x, y, z) -> normalize $ Vector3D (2*x,0.0,2*z)

getNormal Cone     ray loc = case fromVector3D loc of
    (_, 1, _) -> Vector3D (0.0,1.0,0.0)
    (x, y, z) -> normalize $ Vector3D (2*x,-2*y,2*z)

getNormal Cube     ray loc = case fromVector3D loc of
    (_, _, 0) -> Vector3D ( 0.0, 0.0,-1.0) -- front
    (_, _, 1) -> Vector3D ( 0.0, 0.0, 1.0) -- back
    (0, _, _) -> Vector3D (-1.0, 0.0, 0.0) -- left
    (1, _, _) -> Vector3D ( 1.0, 0.0, 0.0) -- right
    (_, 0, _) -> Vector3D ( 0.0,-1.0, 0.0) -- top
    (_, 1, _) -> Vector3D ( 0.0, 1.0, 0.0) -- bottom 
    _         -> error $ "Loc (" ++ show loc ++ ") is not a valid "
                        ++ "cube coordinate for normal... What where you "
                        ++ "thinking?"