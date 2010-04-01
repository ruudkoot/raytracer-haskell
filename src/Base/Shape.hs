{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Base.Shape where

import Base.Shader

import           Data.List                       (sort)
import           Data.Vector

type Intervals = Maybe (Double, Double)

class Shape s f | s -> f where
    getNormal    :: s -> Ray  -> Pt3D -> Vec3D
    getNormal'   :: s -> Pt3D         -> Vec3D 
    inside       :: s -> Pt3D         -> Bool
    intervals    :: Ray -> s -> Intervals
    intervals'   :: Ray -> s -> [Double]
    uv           :: s -> Pt3D         -> SurfaceCoord
    
    getNormal s ray loc = let normal = getNormal' s loc
                           in if inside s (rOrigin ray) 
                              then negate normal
                              else normal
                              
    intervals r s = case intervals' r s of
                     [] -> Nothing
                     [t] -> if t>0.0 then Just (t,t) else Nothing
                     [t1,t2] -> if t2>0.0 then Just (sort2 (t1,t2)) else Nothing
                     ls -> let ts = sort ls in Just (head ls, last ls)

sort2::(Ord a)=>(a,a)->(a,a)
sort2 (x,y) = if x<y then (x,y) else (y,x)

-- | The interval functions return the t's 
-- that solve the following equation:
-- @intersection = eye + t*direction@

