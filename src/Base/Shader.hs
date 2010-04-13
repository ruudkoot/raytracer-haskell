module Base.Shader where

import Data.Colour

type Face            = Int
type SurfaceCoord    = (Face, Double, Double)
data SurfaceProperty = SurfaceProperty
    { surfaceColour                 :: ColourD
    , diffuseReflectionCoefficient  :: Double
    , specularReflectionCoefficient :: Double
    , phongExponent                 :: Double
    }

newtype Shader = Shader { runShader :: SurfaceCoord -> SurfaceProperty }

