{-# LANGUAGE ExistentialQuantification #-}

module Renderer.Scene where

import Data.Angle
import Data.Colour
import Data.Vector
import Data.Matrix (Transformation(..))

import Base.Light
import Base.Shape
import Base.Shader

-- * Types

-- ** World
data World = World 
  {
    wOptions :: RenderOptions
  , wObject  :: Object
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
  , roFov      :: Radians -- fov
  , roWidth    :: Int -- wid
  , roHeight   :: Int -- ht
  , roFile     :: FilePath
  }
  
data Object   = forall s f. Shape s f => Simple s Transformation Shader
              | Union      Object Object
              | Intersect  Object Object
              | Difference Object Object
              --deriving Show


-- | Returns the width and height of 
-- the World as defined in its RenderOptions.
--
getDimensions :: World -> (Int, Int)
getDimensions w = (roWidth opts, roHeight opts)
  where opts = wOptions w

