{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Base.Shape (Shape (..), Intervals) where

import Postlude
import Data.List   (sort)
import Data.Vector
import Base.Shader

type Intervals = Maybe (Double, Double)

class Shape s f | s -> f where
    getNormal    :: s -> Ray  -> Pt3D -> Vec3D
    getNormal'   :: s -> Pt3D         -> Vec3D 
    intervals    :: Ray -> s          -> Intervals
    intervals'   :: s -> Ray          -> [Double]
    uv           :: s -> Pt3D         -> SurfaceCoord
    
    getNormal s ray loc = let normal = getNormal' s loc
                           in if normal !.! (rDirection ray) > 0.0 
                              then negate normal
                              else normal

    -- | The interval functions return the t's 
    -- that solve the following equation:
    -- @intersection = eye + t*direction@
    intervals r s = case intervals' s r of
                      []       -> Nothing
                      [t]      -> if t  > 0.0
                                    then Just (t, positiveInfinity)
                                    else Nothing
                      [t1, t2] -> if t2 > 0.0
                                    then Just (sort2 (t1,t2))
                                    else Nothing
                      ls       -> let ts = sort ls
                                   in Just (head ls, last ls)


