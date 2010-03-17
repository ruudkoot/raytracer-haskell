module Renderer.Intersections where

import Base.Shape

import Control.Applicative

import Data.Ord
import Data.Matrix
import Data.Vector

import Renderer.Scene
import Renderer.UV

import Debug.Trace

type Intersection      = (Double, Double) -- Enters at x, leaves at y

data IntersectionInfo = IntersectionInfo
    { isAHit   :: Bool
    , location :: Pt3D
    , normal   :: Pt3D
    , distance :: Double
    , uv       :: (Double, Double)
    }
    deriving Eq

instance Ord IntersectionInfo where
        compare i i2 = compare (distance i) (distance i2)
        
hit' :: Ray -> Object -> Bool
hit' (Ray o d) (Simple s _ minv _) = hit (Ray (minv !*! o) (minv !*! d)) s
hit' ray       (Union  l r)        = hit' ray l || hit' ray r
hit' ray       (Difference  l r)   = hit' ray l && not (hit' ray r)
hit' ray       (Intersect  l r)    = hit' ray l && hit' ray r

hit :: Ray -> Shape -> Bool
hit r Cube     = let (ox,oy,oz,_) = fromVector4D $ rOrigin r
                     (dx,dy,dz,_) = fromVector4D $ rDirection r
                     calcMinMax o d = let div = 1.0/d
                                          t1 = -o*div
                                          t2 = (1.0-o)*div
                                      in if d >= 0.0 then (t1,t2) else (t2,t1)
                     (txl,txh) = calcMinMax ox dx
                     (tyl,tyh) = calcMinMax oy dy
                     (tzl,tzh) = calcMinMax oz dz
                     tmin = max txl $ max tyl tzl
                     tmax = min txh $ min tyh tzh
                 in tmin < tmax && tmin < 1.0 && tmax > 0.0

hit r Cylinder = let dir = dropW $ Vector4D (1, 0, 1, 0) * rDirection r
                     k = dropW $ Vector4D (1, 0, 1, 0) * rOrigin r
                     a = dir !.! dir
                     b = 2.0 * (k !.! dir)
                     c = (k !.! k) - 1.0
                     d = b*b - 4.0*a*c
                     sqrd = sqrt d
                     t1 = (-b + sqrd)/(2*a)
                     t2 = (-b - sqrd)/(2*a)
                     oy = getY4D $ rOrigin r
                     dy = getY4D $ rDirection r
                     t = min (-oy / dy) ((1 - oy) / dy)
                     t' = max (-oy / dy) ((1 - oy) / dy)
                     sideHit = ((t1 <= t' && t1 >= t) || (t2 <= t' && t2 >= t)) && (t1 < 0 || t2 < 0)
                     bottomHit = magnitudeSquared (k + scaleF t dir) <= 1
                     --topHit = magnitudeSquared (k + dir * (Vector4D (t', t', t', 1))) < 1
                 in sideHit || bottomHit
hit r Sphere   = let dir = dropW $ rDirection r
                     k = dropW $ rOrigin r
                     a = dir !.! dir
                     b = 2.0 * (k !.! dir)
                     c = (k !.! k) - 1.0
                     d = b*b - 4.0*a*c
                     sqrd = sqrt d
                     t1 = (-b + sqrd)/(2*a)
                     t2 = (-b - sqrd)/(2*a)
                  in d >= 0 && (t1 < 0 || t2 < 0)
                        
hit r Cone     = let dir = Vector4D (1, 0, 1, 0) * rDirection r
                     k = Vector4D (1, 0, 1, 0) * rOrigin r
                     a = dir !.! dir - dy * dy
                     b = 2.0 * (k !.! dir) - 2 * oy * dy
                     c = (k !.! k) - oy * oy
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
                 in d >= 0 && sideHit && (t1 < 0 || t2 < 0)
                 -- The 'unit' plane is the XZ plane, so we only have to consider the Y direction.
                 -- If oy == 0, we're in the plane, otherwise we hit it if we move 'downwards' on Y
                 -- when we start 'above' the plane, or vice versa.
hit r Plane    = let oy = getY4D $ rOrigin r
                     dy = getY4D $ rDirection r
                 in (oy == 0) || (oy * dy < 0)
{-
type IntersectionRange = (IntersectionInfo,IntersectionInfo)

intersect' :: Ray -> Object -> Bool
intersect' (Ray o d) (Simple s _ minv _) = intersection (Ray (minv !*! o) (minv !*! d)) s
intersect' ray       (Union  l r)        = intersect' ray l ++ intersect' ray r
intersect' ray       (Difference  l r)   = intersect' ray l && not (intersect' ray r)
intersect' ray       (Intersect  l r)    = intersect' ray l && intersect' ray r
-}

intersection :: Ray -> Shape -> [Intersection]

--Source: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.64.7663&rep=rep1&type=pdf

intersection r Cube    = let (ox,oy,oz,_) = fromVector4D $ rOrigin r
                             (dx,dy,dz,_) = fromVector4D $ rDirection r
                             calcMinMax o d = let div = 1.0/d
                                                  t1 = -o*div
                                                  t2 = (1.0-o)*div
                                              in if d >= 0.0 then (t1,t2) else (t2,t1)
                             (txl,txh) = calcMinMax ox dx
                             (tyl,tyh) = calcMinMax oy dy
                             (tzl,tzh) = calcMinMax oz dz
                             tmin = max txl $ max tyl tzl
                             tmax = min txh $ min tyh tzh
                         in if tmin<=tmax then [(tmin,tmax)] else []

intersection r Cylinder = let dir = dropW $ Vector4D (1, 0, 1, 0) * rDirection r
                              k = dropW $ Vector4D (1, 0, 1, 0) * rOrigin r
                              a = dir !.! dir
                              b = 2.0 * (k !.! dir)
                              c = (k !.! k) - 1.0
                              d = b*b - 4.0*a*c
                              sqrd = sqrt d
                              t1 = (-b + sqrd)/(2*a)
                              t2 = (-b - sqrd)/(2*a)
                              oy = getY4D $ rOrigin r
                              dy = getY4D $ rDirection r
                              delta1 = min (-oy / dy) ((1 - oy) / dy)
                              delta2 = max (-oy / dy) ((1 - oy) / dy)
                              tBottom = delta1 * (magnitudeSquared $ dropW $ rDirection r)
                              tTop = delta2 * (magnitudeSquared $ dropW $ rDirection r)
                              sideHit = (t1 <= tTop && t1 >= tBottom) || (t2 <= tTop && t2 >= tBottom)
                              bottomHit = magnitudeSquared (k + scaleF tBottom dir)
                              topHit = magnitudeSquared (k + scaleF tTop dir)
                              i1 = if topHit < 1.0 then tTop else if t2 <= tTop && t2 >= tBottom then t2 else t1
                              i2 = if bottomHit < 1.0 then tBottom else if t1 <= tTop && t1 >= tBottom then t1 else t2
                          in if i1 < 0 || i2 < 0 then [] else [(i1 `min` i2, i1 `max` i2)]
                          
--Formula from http://www.devmaster.net/wiki/Ray-sphere_intersection, took out the k = (o-c) constant with c = (0,0,0).

intersection r Sphere   = let dir = rDirection r
                              k = rOrigin r
                              a = dir !.! dir
                              b = 2.0 * (k !.! dir)
                              c = (k !.! k) - 1.0
                              d = b*b - 4.0*a*c
                          in trace (show r) $
                             case compare d 0.0 of
                                EQ -> [(-b/(2*a),-b/(2*a))]
                                LT -> []
                                _  -> let sqrd = sqrt d
                                      in [((-b-sqrd)/(2*a), (-b+sqrd)/(2*a))]
                                      -- in [( -)]

intersection r Cone     = undefined
intersection r Plane    = let oy = getY4D $ rOrigin r
                              dy = getY4D $ rDirection r
                          in if (oy == 0) || (oy * dy >= 0) then [] else [(- oy / dy, - oy / dy)]
                          
intersectionInfo :: Ray -> Object -> IntersectionInfo
intersectionInfo ray object = IntersectionInfo
                                { isAHit   = hit' ray object
                                , location = undefined
                                , normal   = undefined
                                , distance = undefined --fst . head $ intersection r Sphere
                                , uv       = (u,v)
                                }
  where (_, u, v) = trace (show its ++ "hit: " ++ (show (hit' ray object))) $
                    case its of
                      []        -> (0, 0, 0)
                      ((a,_):_) -> uvSphere loc
        its = intersection ray Sphere
        loc = (\ v -> let (a, b, c, _) = fromVector4D v in toVec3D a b c) 
                $ instantiate ray (fst $ head its)

-- | Instantiates a ray starting on some point and calculates the ending point
--   given a certain t.
instantiate :: Ray -> Double -> Vec4D
instantiate (Ray origin direction) t = origin + fmap (t *) direction

test (Ray o d) = map (\(x,y) -> (x, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF x d), y, magnitude $ Vector4D (1, 0, 1, 0) * (o + scaleF y d))) (intersection (Ray o d) Cylinder)
