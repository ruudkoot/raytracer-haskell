module Renderer.Intersections where

import Shared.Vector
import Shared.Matrix

import Shared.RenderBase
import Base.Shape

import Renderer.Datatypes
import Data.Ord

type Intersection = (Double, Double) -- Enters at x, leaves at y

hit' :: Ray -> ObjectTree a -> Bool
hit' (Ray o d) (RSimple s _ minv _) = hit (Ray (minv !*! o) (minv !*! d)) s
hit' ray       (RUnion  l r)        = hit' ray l || hit' ray r
hit' ray       (RDifference  l r)   = hit' ray l && not (hit' ray r)
hit' ray       (RIntersect  l r)    = hit' ray l && (hit' ray r)

hit :: Ray -> Shape -> Bool
hit r Cube     = undefined
hit r Cylinder = let dir = Vector4D (1, 0, 1, 0) * rDirection r
                     k = Vector4D (1, 0, 1, 0) * rOrigin r
                     a = dir <.> dir
                     b = 2.0 * (k <.> dir)
                     c = (k <.> k) - 1.0
                     d = b*b - 4.0*a*c
                     sqrd = sqrt d
                     t1 = (-b + sqrd)/(2*a)
                     t2 = (-b - sqrd)/(2*a)
                     oy = getY4D $ rOrigin r
                     dy = getY4D $ rDirection r
                     t = min (-oy / dy) ((1 - oy) / dy)
                     t' = max (-oy / dy) ((1 - oy) / dy)
                     sideHit = (t1 <= t' && t1 >= t) || (t2 <= t' && t2 >= t)
                     bottomHit = magnitudeSquared (k + dir * (Vector4D (t, t, t, 1))) <= 1
                     --topHit = magnitudeSquared (k + dir * (Vector4D (t', t', t', 1))) < 1
                 in (sideHit || bottomHit)
hit r Sphere   = let dir = dropW $ rDirection r
                     k = dropW $ rOrigin r
                     a = dir <.> dir
                     b = k <.> dir
                     c = (k <.> k) - 1.0
                     d = b*b - a*c
                  in d >= 0
                        
hit r Cone     = let dir = Vector4D (1, 0, 1, 0) * rDirection r
                     k = Vector4D (1, 0, 1, 0) * rOrigin r
                     a = dir <.> dir - dy * dy
                     b = 2.0 * (k <.> dir) - 2 * oy * dy
                     c = (k <.> k) - oy * oy
                     d = b*b - 4.0*a*c
                     sqrd = sqrt d
                     t1 = (-b + sqrd)/(2*a)
                     t2 = (-b - sqrd)/(2*a)
                     oy = getY4D $ rOrigin r
                     dy = getY4D $ rDirection r
                     t = min (-oy / dy) ((1 - oy) / dy)
                     t' = max (-oy / dy) ((1 - oy) / dy)
                     sideHit = (t1 <= t' && t1 >= t) || (t2 <= t' && t2 >= t)
                     --bottomHit = magnitudeSquared (k + dir * (Vector4D (t, t, t, 1))) <= 1
                     --topHit = magnitudeSquared (k + dir * (Vector4D (t', t', t', 1))) < 1
                 in (sideHit)
                 -- The 'unit' plane is the XZ plane, so we only have to consider the Y direction.
                 -- If oy == 0, we're in the plane, otherwise we hit it if we move 'downwards' on Y
                 -- when we start 'above' the plane, or vice versa.
hit r Plane    = let oy = getY4D $ rOrigin r
                     dy = getY4D $ rDirection r
                 in (oy == 0) || (oy * dy < 0)


hitSquareZ::Ray->Bool
hitSquareZ r = let (ox,oy,oz,_) = fromVector4D $ rOrigin r
                   (dx,dy,dz,_) = fromVector4D $ rDirection r
               in if (oz == 0) || (oz * dz >= 0) 
                  then False
                  else let t = -oz/dz
                           u = ox + t*dx
                           v = oy + t*dy
                       in u < 1.0 && v < 1.0
                
intersection :: Ray -> Shape -> [Intersection]
intersection r Cube     = undefined
intersection r Cylinder = undefined

--Formula from http://www.devmaster.net/wiki/Ray-sphere_intersection, took out the k = (o-c) constant with c = (0,0,0).

intersection r Sphere   = let dir = rDirection r
                              k = rOrigin r
                              a = dir <.> dir
                              b = 2.0 * (k <.> dir)
                              c = (k <.> k) - 1.0
                              d = b*b - 4.0*a*c
                          in case compare d 0.0 of
                                EQ -> [(-b/(2*a),-b/(2*a))]
                                LT -> []
                                _  -> let sqrd = sqrt d
                                      in [((-b-sqrd)/(2*a), (-b+sqrd)/(2*a))]

intersection r Cone     = undefined
intersection r Plane    = let oy = getY4D $ rOrigin r
                              dy = getY4D $ rDirection r
                          in if (oy == 0) || (oy * dy >= 0) then [] else [(- oy / dy, - oy / dy)]
