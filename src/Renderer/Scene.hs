{-# LANGUAGE ExistentialQuantification #-}

module Renderer.Scene where

import Data.Angle
import Data.Colour
import Data.Transformation

import Base.Light
import Base.Shape
import Base.Shader

import Base.BoundingSphere
-- * Types

-- ** World
data Scene = Scene
  { options :: Options
  , object  :: Object
  , lights  :: [Light] }
  

-- | The global datatype, also referenced to as `scene'. We pushed down some
-- of the parameters as stated in the render function of gml for ease. We are
-- not entirely sure whether the `union' representation of the objects is 
-- workable for us.

data Options = Options
  { ambience :: ColourD
  , depth    :: Int
  , fov      :: Radians
  , width    :: Int
  , height   :: Int
  , file     :: FilePath }
  
data Object = forall s f. Shape s f => Simple     s Transformation Shader BSphere
            |                          Union      Object Object BSphere
            |                          Intersect  Object Object BSphere
            |                          Difference Object Object BSphere



-- | Returns the width and height of 
-- the World as defined in its RenderOptions.
--
getDimensions :: Scene -> (Int, Int)
getDimensions w = (width opts, height opts)
  where opts = options w

