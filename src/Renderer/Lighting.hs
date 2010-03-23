-- | Contains the code for applying local lighting. It supports `dynamically'
--   adding of more lighting methods provided the information provided
module Renderer.Lighting (localLighting) where

import Base.Light  
import Base.Shader
  
import Data.Colour (ColourD, Colour(..), fromColour)
import Data.Vector
  
import Renderer.Intersections2
import Renderer.Scene

  
import Control.Applicative ((<$>))


localLighting :: Vec3D -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> Vec3D -> Vec3D
localLighting amb its lights surface r reflected = diffuse + specular
  where diffuse     = (kd *) <$> amb * c + sum (map dirLight lights)
        specular    = (ks *) <$> reflected * c + sum (map phong lights)
        kd          = diffuseReflectionCoefficient surface
        ks          = specularReflectionCoefficient surface
        dirLight l  = ((n !.! dir l) *) <$> intensity l * c
        phong l     = ((n !.! dirhalf l) ** phongExponent surface *) <$> intensity l * c
        dirhalf l   = normalize (dropW (rDirection r) `cross` dir l) -- ?
        c           = fromColour $ surfaceColour surface
        n           = normal its
        loc         = location its
        intensity l = getIntensity l loc
        dir         = direction loc
 

-- | Get the unit vector from a location 
-- to a RenderLight's position.
--
direction :: Pt3D -> RenderLight -> Vec3D
direction loc (SpotLight   pos _ _ _ _ ) = normalize $ pos - loc
direction loc (PointLight  pos _       ) = normalize $ pos - loc
direction _   (DirectLight dir _       ) = normalize $ negate dir


-- | Calculate the intensity of RenderLight 
-- at a certain position. 
--
getIntensity :: RenderLight -> Pt3D -> Vec3D               
getIntensity (DirectLight _  i) _ = i 
getIntensity (PointLight pos i) loc = attenuate (magnitude $ abs (loc - pos)) i
getIntensity (SpotLight pos at i cutoff exp) loc = attenuate (magnitude $ abs (loc - pos)) i'
  where i' = if angle > cutoff then toVec3D 0 0 0 else spot 
        spot = (((dir / abs dir) !.! (posDir / abs posDir)) ** exp *) <$> i
        dir = at - pos 
        posDir = loc - pos
        angle = acos(dir !.! posDir)


-- | The light from point lights and spotlights is 
-- attenuated by the distance from the light to 
-- the surface.
-- 
attenuate :: Double -> Vec3D -> Vec3D
attenuate d = fmap ((/ dis) . (100*))
  where dis = 99 + d ** 2

