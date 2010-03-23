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
        kdsum = (kd *) <$> sum (map (\l -> ((n !.! direction l) *) <$> intensity l * c) lights)
        ksisc = (ks *) <$> reflected * c
        kssum = (ks *) <$> sum (map (\l -> ((n !.! dirhalf l) ** phongExponent surface *) <$> intensity l * c) lights)
        kd = diffuseReflectionCoefficient surface
        ks = specularReflectionCoefficient surface
        n = normal its
        (Colour c) = surfaceColour surface
        intensity l = getIntensity l (location its) 
        direction (PointLight pos _) = normalize $ pos - location its
        direction (DirectLight dir _) = normalize $ negate dir
        direction (SpotLight pos _ _ _ _) = normalize $ pos - location its
        dirhalf l = normalize (dropW (rDirection r) `cross` direction l) -- not sure about this
        eye = toVec3D 0 0 (-1)
 
getIntensity :: RenderLight -> Pt3D -> Vec3D               
getIntensity (DirectLight _  i) _ = i 
getIntensity (PointLight pos i) loc = attenuate i (magnitude $ abs (loc - pos))
getIntensity (SpotLight pos at i cutoff exp) loc = attenuate i' (magnitude $ abs (loc - pos))
  where i' = if angle > cutoff then toVec3D 0 0 0 else spot 
        spot = (((dir / (abs dir)) !.! (posDir / (abs posDir))) ** exp *) <$> i
        dir = at - pos 
        posDir = loc - pos
        angle = acos(dir !.! posDir)


attenuate :: Vec3D -> Double -> Vec3D
attenuate i d = fmap (\x -> 100 * x / (99 + d ** 2)) i 

