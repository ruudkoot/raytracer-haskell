module Renderer.Intersections2 where
------------------------------------------------------------------------------
import Base.Shape

import Data.Vector

import Renderer.Scene
import Renderer.UV

import Debug.Trace
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- * Datastructures
------------------------------------------------------------------------------
data IntersectionInfo = IntersectionInfo
    { isHit      :: Bool
    , location :: Pt3D -- Real world location
    , normal   :: Pt3D -- Real world normal
    , distance :: Double -- ??
    , textureCoord :: (Int, Double, Double) -- unit world coord
    , tees     :: [Intersection]
    }
    deriving Eq

type Intersection = (Double, Double)
------------------------------------------------------------------------------
-- * Intersection Wrapper functions
------------------------------------------------------------------------------
intersect :: Ray -> Object -> IntersectionInfo
-- ** Sphere
intersect ray obj@(Simple (Sphere) m1 m2 shader) = 
  IntersectionInfo
    { isHit        = not $ null its
    , location     = loc --error "Don't have locations yet"
    , normal       = error "Don' 'v' normals 't"
    -- , distance     = error "Don' 'v' distance 't"--undefined --fst . head $ intersection r Sphere
    , distance     = fst . head $ its
    , textureCoord = textcoord
    , tees         = its
    }
 where textcoord = trace (show its ++ "hit: " ++ (show (hit ray obj))) $
                   case its of
                     []        -> (0, 0, 0)
                     ((a,_):_) -> uvSphere loc
       its = intervals ray Sphere
       loc = (\ v -> let (a, b, c, _) = fromVector4D v in toVec3D a b c) 
               $ instantiate ray (fst $ head its)

-- ** Plane

-- ** Cube

-- ** Cylinder

------------------------------------------------------------------------------
-- * Intersection Inner functions
------------------------------------------------------------------------------
intervals :: Ray -> Shape -> [Intersection]


-- ** Sphere
intervals r Sphere   = let dir = dropW $ rDirection r
                           k = dropW $ rOrigin r
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

-- ** Plane
intervals r Plane    = let oy = getY4D $ rOrigin r
                           dy = getY4D $ rDirection r
                       in if (oy == 0) || (oy * dy >= 0)
                           then []
                           else [(- oy / dy, - oy / dy)]
-- ** Cube

-- ** Cylinder

------------------------------------------------------------------------------
-- * Hit Wrapper functions
-- Let lazyness handle efficiency for now
------------------------------------------------------------------------------
hit :: Ray -> Object -> Bool
hit r o = isHit $ intersect r o


------------------------------------------------------------------------------
-- * Util functions
-- Let lazyness handle efficiency for now
------------------------------------------------------------------------------
-- | Instantiates a ray starting on some point and calculates the ending point
--   given a certain t.
instantiate :: Ray -> Double -> Vec4D
instantiate (Ray origin direction) t = origin + fmap (t *) direction