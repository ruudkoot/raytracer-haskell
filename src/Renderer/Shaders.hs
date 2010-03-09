-- | The Renderer.Shaders module contains our basic shaders, these are used
--   for testing purposes.
module Renderer.Shaders where

import Data.Colour

import Base.Shader


-- | Basic colors.
red, green, blue :: Shader
red   = solid 1.0  0    0  
green = solid 0    1.0  0  
blue  = solid 0    0    1.0

solid :: Double -> Double -> Double -> Shader
solid r g b = Shader (\_ -> ShaderResult (Colour (r,g,b)) 1.0 0.0 1.0)


-- * 2D shaders

-- | Gradient function
gradient :: ColourD -> ColourD -> Shader
gradient c1 c2 = undefined

-- | Noise shaders
perlin :: ColourD -> ColourD -> Shader
perlin c1 c2 = undefined 

-- * 3D shaders:
--

-- | Generates a shader that produces a colour that's linearly dependent on
--   the 3 coordinates of the system.
gradient3D :: ColourD -> ColourD -> ColourD -> Shader
gradient3D c1 c2 c3 = undefined

