-- | Contains the code for applying local lighting. It supports `dynamically'
--   adding of more lighting methods provided the information provided <-- what?
module Renderer.Lighting (localLighting) where

import Base.Light (RenderLight(..))
import Base.Shader (SurfaceProperty(..))
  
import Data.Colour (Colour, fromColour)
import Data.Vector 

import Renderer.Intersections
import Renderer.IntersectionInfo

import Control.Applicative ((<$>))


-- Calculate the local lighting.
-- This basically implements the lighting model 
-- from page 11 of the assigment.
--
localLighting :: Vec3D -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> Vec3D -> Vec3D
localLighting ambient its lights surface r reflected = diffuse + specular
  where diffuse    = col (diffuseReflectionCoefficient surface) ambient dirLight
        specular   = col (specularReflectionCoefficient surface) reflected phong
        col k i f  = (k*) `vmap` i * surfC + sum (map f lights)

        dirLight l = light (n !.! dir l) l 
        phong    l = light ((n !.! dirhalf l) ** phongExponent surface) l
        light  f l = (f*) `vmap` getIntensity l (location its) * surfC

        dirhalf  l = normalize ((rDirection r) `cross` dir l) -- ?
        dir        = direction (location its)
        
        surfC      = fromColour $ surfaceColour surface
        n          = normal its
 

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
        spot = (((dir / abs dir) !.! (posDir / abs posDir)) ** exp *) `vmap` i
        dir = at - pos 
        posDir = loc - pos
        angle = acos(dir !.! posDir)


-- | The light from point lights and spotlights is 
-- attenuated by the distance from the light to 
-- the surface.
-- 
attenuate :: Double -> Vec3D -> Vec3D
attenuate d = vmap ((/ dis) . (100*))
  where dis = 99 + d ** 2

