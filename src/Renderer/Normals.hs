module Renderer.Normals where

import Base.Shape
import Data.Vector
import Renderer.Scene (Ray(..))

-- TODO: Incorporate the ray


getNormal :: Shape -> Ray -> Pt3D -> Vec3D 

getNormal s ray loc = let normal = getNormal' s loc
                      in if inside s (dropW $ rOrigin ray) 
                         then negate normal
                         else normal

getNormal' :: Shape -> Pt3D -> Vec3D 

getNormal' Sphere   loc = normalize loc

getNormal' Plane    loc = Vector3D (0.0,1.0,0.0)

getNormal' Cylinder loc = case fromVector3D loc of
    (_, 0, _) -> Vector3D (0.0,-1.0,0.0)
    (_, 1, _) -> Vector3D (0.0, 1.0,0.0)
    (x, y, z) -> normalize $ Vector3D (2*x,0.0,2*z)

getNormal' Cone     loc = case fromVector3D loc of
    (_, 1, _) -> Vector3D (0.0,1.0,0.0)
    (x, y, z) -> normalize $ Vector3D (2*x,-2*y,2*z)

getNormal' Cube     loc = case fromVector3D loc of
    (_, _, 0) -> Vector3D ( 0.0, 0.0,-1.0) -- front
    (_, _, 1) -> Vector3D ( 0.0, 0.0, 1.0) -- back
    (0, _, _) -> Vector3D (-1.0, 0.0, 0.0) -- left
    (1, _, _) -> Vector3D ( 1.0, 0.0, 0.0) -- right
    (_, 0, _) -> Vector3D ( 0.0,-1.0, 0.0) -- top
    (_, 1, _) -> Vector3D ( 0.0, 1.0, 0.0) -- bottom 
    _         -> error $ "Loc (" ++ show loc ++ ") is not a valid "
                        ++ "cube coordinate for normal... What where you "
                        ++ "thinking?"

inside::Shape -> Pt3D -> Bool

inside Sphere   loc = magnitude loc < 1.0

inside Plane    loc = getY3D loc < 0.0

inside Cone     loc = let (x,y,z) = fromVector3D loc
                      in y < 1.0 && x*x+z*z-y*y < 0.0

inside Cylinder loc = let (x,y,z) = fromVector3D loc
                      in y < 1.0 && y > 0.0 && x*x+z*z < 0.0

inside Cube     loc = let (ox,oy,oz) = fromVector3D loc
                      in ox > 0.0 && ox < 1.0 && oy > 0.0 && oy < 1.0 && oz > 0.0 && oz < 1.0
