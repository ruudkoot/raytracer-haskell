module Renderer.Normals where

import Base.Shape
import Data.Vector
import Renderer.Scene (Ray(..))

-- TODO: Incorporate the ray


getNormal :: Shape -> Ray -> Pt3D -> Vec3D 

getNormal s ray loc = let normal = getNormal' s loc
                      in if inside s (dropW $ rOrigin ray) 
                         then normal
                         else negate normal

getNormal' :: Shape -> Pt3D -> Vec3D 

getNormal' Sphere   loc = normalize loc

getNormal' Plane    _   = Vector3D (0.0,1.0,0.0)

getNormal' Cylinder (Vector3D (x,y,z)) | 0.0 `dEq` y = Vector3D (0.0,-1.0,0.0)
                                       | 1.0 `dEq` y = Vector3D (0.0, 1.0,0.0)
                                       | otherwise   = normalize $ Vector3D (2*x,0.0,2*z)

getNormal' Cone     (Vector3D (x,y,z)) | 1.0 `dEq` y = Vector3D (0.0,1.0,0.0)
                                       | otherwise   = normalize $ Vector3D (2*x,-2*y,2*z)

getNormal' Cube     (Vector3D (x,y,z)) | 0.0 `dEq` z = Vector3D ( 0.0, 0.0,-1.0) -- front
                                       | 1.0 `dEq` z = Vector3D ( 0.0, 0.0, 1.0) -- back
                                       | 0.0 `dEq` x = Vector3D (-1.0, 0.0, 0.0) -- left
                                       | 1.0 `dEq` x = Vector3D ( 1.0, 0.0, 0.0) -- right
                                       | 0.0 `dEq` y = Vector3D ( 0.0,-1.0, 0.0) -- top
                                       | 1.0 `dEq` y = Vector3D ( 0.0, 1.0, 0.0) -- bottom 
                                       | otherwise = error $ "Loc (" ++ show (Vector3D (x,y,z)) ++ ") is not a valid "
                                                          ++ "cube coordinate for normal... What where you "
                                                          ++ "thinking?"

dEq::Double -> Double -> Bool
dEq d1 d2 = abs (d1-d2) < 0.001

inside::Shape -> Pt3D -> Bool

inside Sphere   loc = magnitude loc < 1.0

inside Plane    loc = getY3D loc < 0.0

inside Cone     loc = let (x,y,z) = fromVector3D loc
                      in y < 1.0 && y > 0.0 && x*x+z*z-y*y < 0.0

inside Cylinder loc = let (x,y,z) = fromVector3D loc
                      in y < 1.0 && y > 0.0 && x*x+z*z < 0.0

inside Cube     loc = let (ox,oy,oz) = fromVector3D loc
                      in ox > 0.0 && ox < 1.0 && oy > 0.0 && oy < 1.0 && oz > 0.0 && oz < 1.0
