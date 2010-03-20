-- | Contains the code for applying local lightning. It supports `dynamically'
--   adding of more lightning methods provided the information provided

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- ERRR: Er is hier nu een  hoop gekut met het heen en weer vertalen tussen
--       Point3D en ColourD omdat we gebruik maken van de gesharede lights
--       datastructuur. Als we dit hier mooier willen hebben moeten we
--       misschien toch de structuren scheiden?
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module Renderer.Lightning (localLightning) where

import Base.Light  
import Base.Shader
  
import Data.Colour
import Data.Vector
  
import Renderer.Intersections2
import Renderer.Scene

  
-- | Applies a list of color transformations using the int. info, the 
--   available lights (TODO: Where do we do occlusion testing??? Should this
--   also contain soft shadows???) for the moment this assumes that the
--   provided lights are the visible lights. :)
--   TODO: Ambient color
--   TODO: Shadows, maybe [(Factor, RenderLight)]  to indicate how heavy a
--         light weighs.
localLightning :: IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
localLightning = local
  -- in surfaceColour surfaceproperty
  

local :: IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
local isect lights surface ray =
  toColour . sum $ map (\l -> diffuse l + specular l) lights
  where
    ----
    diffuse (PointLight pos col) = 
      let n       = normal isect
          l       = normalize $ pos - location isect
          angle   = n !.! l
          dRC     = diffuseReflectionCoefficient surface
          baseCol = fromColour $ surfaceColour surface 
      in fmap (angle * ) (fmap (dRC*) col * baseCol)
    diffuse (DirectLight dir col) = 
      let n       = normal isect
          angle   = n !.! dir
          dRC     = diffuseReflectionCoefficient surface
          baseCol = fromColour $ surfaceColour surface 
      in fmap (angle * ) (fmap (dRC*) col * baseCol)
    diffuse _ = toVec3D 0 0 0
    -- diffuse l = error $ "No diffuse implementation yet for this light: "
    --                     ++ show l
    ----
    specular (PointLight pos col) =
      let n     = normal isect
          l     = normalize $ pos - location isect
          angle = n !.! l
          dir   = dropW $ rDirection ray
          r     = normalize $ fmap (2*angle*) n - l
          v     = normalize $ fmap negate dir
          factor = max (r !.! v) 0 ** phongExponent surface
      in fmap (factor * specularReflectionCoefficient surface *) col
    specular _ = toVec3D 0 0 0
    -- specular l = error $ "No specular implementation yet for this light: "
    --                       ++ show l


