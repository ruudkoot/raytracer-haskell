-- | Contains the code for applying local lightning. It supports `dynamically'
--   adding of more lightning methods provided the information provided

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- ERRR: Er is hier nu een  hoop gekut met het heen en weer vertalen tussen
--       Point3D en ColourD omdat we gebruik maken van de gesharede lights
--       datastructuur. Als we dit hier mooier willen hebben moeten we
--       misschien toch de structuren scheiden?
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module Renderer.Lightning where

import Base.Light  
import Base.Shader
  
import Data.Colour
import Data.Vector
  
import Renderer.Intersections2
  
-- | Applies a list of color transformations using the int. info, the 
--   available lights (TODO: Where do we do occlusion testing??? Should this
--   also contain soft shadows???) for the moment this assumes that the
--   provided lights are the visible lights. :)
--   TODO: Ambient color
localLightning :: IntersectionInfo -> [RenderLight] -> SurfaceProperty -> ColourD
localLightning intersect lights surfaceproperty = 
  let diff = diffuse intersect lights surfaceproperty
  -- in (fresnel . specular) diff -- Gaat pas werken als we de normaal hebben :)
                               -- En de location en ...
  in surfaceColour surfaceproperty
  

diffuse :: IntersectionInfo -> [RenderLight] -> SurfaceProperty -> ColourD
diffuse intersect lights surfaceproperty =
  sum $ map diffuse' lights
  where
    diffuse' (PointLight pos colour) = 
      let l       = normalize $ pos - location intersect
          ndotl   = normal intersect !.! l
          dRC     = diffuseReflectionCoefficient surfaceproperty
          baseCol = toVector $ surfaceColour surfaceproperty 
      in Data.Colour.fromVector $ 
           fmap (ndotl * )  
                (fmap (dRC*) colour * baseCol)
    diffuse' l = error $ "No diffuse implementation yet for this light: "
                         ++ show l

specular = id
fresnel = id