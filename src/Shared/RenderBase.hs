module Shared.RenderBase where

import Base.Light
import Base.Shape

import Shared.Colour
import Shared.Vector
import Shared.Matrix (Matrix4D)

-- * Types

-- ** Shaders
type ShaderCoord = (Double,Double,Int)
data ShaderResult = ShaderResult
    {
     srColor::ColourD
    ,srKd::Double
    ,srKs::Double
    ,srPhong::Double
    }

newtype Shader = Shader (ShaderCoord -> ShaderResult)
instance Show Shader where
    show s = "Shader function"
instance Eq Shader where
    (==) a b = error "Alessandro" --True
    

-- ** World
data World surface = World 
  {
    wOptions :: RenderOptions
  , wObject  :: ObjectTree surface
  , wLights  :: [RenderLight]
  }
  


-- | The global datatype, also referenced to as `scene'. We pushed down some
-- of the parameters as stated in the render function of gml for ease. We are
-- not entirely sure whether the `union' representation of the objects is 
-- workable for us.

data RenderOptions = RenderOptions
  {
    roAmbience :: ColourD -- amb
  , roDepth    :: Int
  , roFov      :: Double -- fov
  , roWidth    :: Int -- wid
  , roHeight   :: Int -- ht
  , roFile     :: FilePath
  }
  

data Ray = Ray 
  {
    rOrigin    :: Vec4D
  , rDirection :: Vec4D
  }


  
data ObjectTree a = RSimple Shape (Matrix4D Double) (Matrix4D Double) a -- Shape, transformation and inverse of transformation matrix
                  | RUnion (ObjectTree a) (ObjectTree a)
                  | RIntersect (ObjectTree a) (ObjectTree a)
                  | RDifference (ObjectTree a) (ObjectTree a)
