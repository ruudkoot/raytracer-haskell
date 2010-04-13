{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Base.Shape (Shape (..), Intervals) where

import Postlude
import Data.Vector
import Base.Shader

import Base.BoundingSphere

type Intervals = Maybe (Double, Double)

class (Enum f) => Shape s f | s -> f where
    getNormal    :: s -> Ray  -> Pt3D -> Vec3D
    getNormal'   :: s -> Pt3D         -> Vec3D 
    intervals    :: Ray -> s          -> Intervals
    intervals'   :: s -> Ray          -> [Double]
    uv'          :: s -> Pt3D         -> (f, Double, Double)
<<<<<<< HEAD:src/Base/Shape.hs
    boundingBox  :: s -> Bbox
    boundingSphere :: s -> BSphere
=======
>>>>>>> 5aa912faa5275ff96ce3ec6f460a39cc1a398c17:src/Base/Shape.hs
    
    getNormal s ray loc = let normal = getNormal' s loc
                           in if normal !.! rDirection ray > 0.0 
                              then negate normal
                              else normal

    -- | The interval functions return the t's 
    -- that solve the following equation:
    -- @intersection = eye + t*direction@
    intervals r s = case intervals' s r of
                      []       -> Nothing
                      [t]      -> if t > 0.0 
                                  then Just (t, t)                                  
                                  else Nothing
                      [t1, t2] -> let (ts1,ts2) = sort2 (t1,t2)
                                  in if ts2 < 0.0
                                     then Nothing
                                     else Just (ts1,ts2)
                      _        -> error "Oh shit" --let ts = sort ls
                                  --  in Just (head ls, last ls)

    uv :: s -> Pt3D -> SurfaceCoord
    uv s p = let (f, x, y) = uv' s p in (fromEnum f, x, y)
<<<<<<< HEAD:src/Base/Shape.hs

    boundingBox _ = Bbox (toVec3D 0.0 0.0 0.0) (toVec3D 1.0 1.0 1.0)
    boundingSphere _ = BSphere (toVec3D 0.0 0.0 0.0) 2.0
=======
>>>>>>> 5aa912faa5275ff96ce3ec6f460a39cc1a398c17:src/Base/Shape.hs
