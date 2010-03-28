-- | The Renderer.Shaders module contains our basic shaders, these are used
--   for testing purposes.
module Renderer.Shaders where

import Base.Shader
import Data.Colour
import Data.Vector


-- | Basic colors.
red, green, blue :: Shader
red   = solid 1.0  0    0  
green = solid 0    1.0  0  
blue  = solid 0    0    1.0

solid :: Double -> Double -> Double -> Shader
solid r g b = Shader ( const SurfaceProperty { surfaceColour                 = colour r g b
                                             , diffuseReflectionCoefficient  = 1.0
                                             , specularReflectionCoefficient = 0.5
                                             , phongExponent                 = 1.0
                                             }
                     )


-- * 2D shaders

-- | Gradient function
gradient :: Colour -> Colour -> Shader
gradient c1 c2 = undefined

-- | Noise shaders
perlin :: Colour -> Colour -> Shader
perlin c1 c2 = undefined 

-- * 3D shaders:
--

-- | Generates a shader that produces a colour that's linearly dependent on
--   the 3 coordinates of the system.
gradient3D :: Colour -> Colour -> Colour -> Shader
gradient3D c1 c2 c3 = undefined


{------------------------------------------------------------------------------}

uvShader :: Shader
uvShader = Shader { runShader = 
  \(face, u, v) -> SurfaceProperty { 
                     surfaceColour = colour (u - fromIntegral (floor u))
                                            (v - fromIntegral (floor v))
                                            0
                     , diffuseReflectionCoefficient  = 1.0
                     , specularReflectionCoefficient = 1.0
                     , phongExponent                 = 1.0
                     }
                  }

