module Base.BoundingSphere where

import Data.Vector
import Postlude

data BSphere = BSphere Vector Double

infiniteSphere::BSphere
infiniteSphere = BSphere (toVec3D 0.0 0.0 0.0) positiveInfinity

bSphereIntersect::Ray -> BSphere -> [Double]
bSphereIntersect ray (BSphere loc rad) = 
    let (k, dir) = (rOrigin ray - loc, rDirection ray)
        a        = dot dir
        b        = 2.0 * (k !.! dir)
        c        =  (dot k - rad*rad)
    in solveQuadratic a b c

unionSpheres::BSphere -> BSphere -> BSphere
unionSpheres (BSphere pos1 r1) (BSphere pos2 r2) =
    let dir = pos2 - pos1
        dist = magnitude dir
        newradius = 0.5*(dist+r1+r2)
        midoff = newradius - r1
        newpos = pos1 + ((midoff*) `vmap` (normalize dir))
    in BSphere newpos newradius 

intersectSpheres::BSphere -> BSphere -> BSphere
intersectSpheres (BSphere pos1 r1) (BSphere pos2 r2) =
    let dir = pos2 - pos1
        dist = magnitude dir
        s2dist = dist - r1
        s1dist = dist - r2
        overlap = dist - s1dist - s2dist
        midoff = s2dist + overlap*0.5
        newpos = pos1 + ((midoff*) `vmap` (normalize dir))
    in if overlap < 0.0 then BSphere (toVec3D 0.0 0.0 0.0) 0.0
       else BSphere newpos (max r1 r2)

differenceSpheres::BSphere -> BSphere -> BSphere
differenceSpheres = const

translateSphere::Double->Double->Double -> BSphere -> BSphere
translateSphere x y z (BSphere pos r) = BSphere (pos+(toVec3D x y z)) r

rotateSphere::Double -> BSphere ->  BSphere
rotateSphere = flip const

scaleSphere:: Double -> BSphere -> BSphere
scaleSphere s (BSphere pos r) = BSphere pos (r*s)
