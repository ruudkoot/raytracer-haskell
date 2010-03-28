module Base.Shader where

import Data.Colour

type Face            = Int
type SurfaceCoord    = (Face, Double, Double)
data SurfaceProperty = SurfaceProperty
    { surfaceColour                 :: Colour
    , diffuseReflectionCoefficient  :: Double
    , specularReflectionCoefficient :: Double
    , phongExponent                 :: Double
    }

newtype Shader = Shader { runShader :: SurfaceCoord -> SurfaceProperty }

instance Show Shader where
    show = const "<<shader>>"

instance Eq Shader where
    (==) = error "cannot compare equality of shaders"
