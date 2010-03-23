-- | Contains the code for applying local lighting. It supports `dynamically'
--   adding of more lighting methods provided the information provided
module Renderer.Lighting (localLighting) where

import Base.Light  
import Base.Shader
  
import Data.Colour
import Data.Vector
  
import Renderer.Intersections2
import Renderer.Scene

  
import Control.Applicative ((<$>))


localLighting :: ColourD -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> Vec3D -> Vec3D
localLighting (Colour amb) its lights surface r reflected = kdiac + kdsum + kssum + ksisc
  where kdiac = (kd *) <$> amb * c
        kdsum = (kd *) <$> sum (map (\l -> ((n !.! direction l) *) <$> getIntensity l * c) lights)
        ksisc = (ks *) <$> reflected * c
        kssum = (ks *) <$> sum (map (\l -> ((n !.! dirhalf l) ** phongExponent surface *) <$> getIntensity l * c) lights)
        kd = diffuseReflectionCoefficient surface
        ks = specularReflectionCoefficient surface
        n = normal its
        (Colour c) = surfaceColour surface
        getIntensity (DirectLight _ i) = i 
        getIntensity (PointLight  _ i) = i
        getIntensity (SpotLight _ _ i _ _) = i
        direction (PointLight pos _) = normalize $ pos - location its
        direction (DirectLight dir _) = normalize $ negate dir
        direction (SpotLight pos _ _ _ _) = normalize $ pos - location its
        dirhalf l = normalize (dropW (rDirection r) `cross` direction l) -- not sure about this
        eye = toVec3D 0 0 (-1)
                
