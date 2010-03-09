module Renderer.Scene where

import Data.Colour
import Data.Vector
import Data.Matrix (Matrix4D)

import Base.Light
import Base.Shape

-- * Types

-- ** World
data World surface = World 
  {
    wOptions :: RenderOptions
  , wObject  :: Object surface
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


  
data Object a = Simple Shape (Matrix4D Double) (Matrix4D Double) a -- Shape, transformation and inverse of transformation matrix
              | Union      (Object a) (Object a)
              | Intersect  (Object a) (Object a)
              | Difference (Object a) (Object a)
              deriving Show
