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
--   TODO: Shadows, maybe [(Factor, RenderLight)]  to indicate how heavy a
--         light weighs.
--         Filter out invisible lights.
localLightning :: ColourD -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
localLightning ambient its lights surface r = 
  add (times (surfaceColour surface) ambient)
      (local its lights surface r)


local :: IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
local isect lights surface ray = toColour . sum $ map (\l -> d l + s l) lights
  where d l = diffuse isect l surface
        s l = specular isect l surface ray

diffuse :: IntersectionInfo -> RenderLight -> SurfaceProperty -> Vec3D
diffuse isect light surface = case light of 
  (PointLight  pos col) -> let l = lVector isect pos in doLight (n !.! l) col
  (DirectLight dir col) -> doLight (n !.! dir) col 
  _                     -> toVec3D 0 0 0
  where
    n = normal isect
    baseCol = fromColour $ surfaceColour surface 
    dRC     = diffuseReflectionCoefficient surface
    doLight angle col = fmap (angle * dRC *) col * baseCol

lVector isect pos = normalize $ pos - location isect

specular :: IntersectionInfo -> RenderLight -> SurfaceProperty -> Ray -> Vec3D 
specular isect light surface ray = case light of 
  (PointLight pos col) -> let l = lVector isect pos 
                          in doLight (r l !.! v) col
  _                    -> toVec3D 0 0 0 
  where n = normal isect
        angle l = n !.! l
        dir   = dropW $ rDirection ray
        r l    = normalize $ fmap (2 * angle l *) n - l
        v     = normalize $ negate dir
        factor angle = max angle 0 ** phongExponent surface
        sRC = specularReflectionCoefficient surface
        doLight angle col = fmap (factor angle * sRC *) col


-- | Transformed version of the formula in the assignment on page 11.
-- local' :: ColourD -> IntersectionInfo -> [RenderLight] -> SurfaceProperty -> Ray -> ColourD
-- local' ambient its lights surface r =
--   surfaceColor * $   diffRC * ambient 
--                    + totalIntensity * (diffRC * diffuse + specRC * specular)
--                    -- + specRC * toVec3D 0 0 0 -- TODO: Reflection, add the 
--                                                --       colour of the resulting
--                                                --       ray to the parameters
--   where totalIntensity         = (sum . map getIntensity) lights                
--         diffRC                 = diffuseReflectionCoefficient surface
--         specRC                 = specularReflectionCoefficient surface
--         diffuse                = (sum . map (\l -> n !.! getIntensity l )) lights
--         specular               = (sum . map (\l -> n !.! ))
--         n                      = normal isect
--         l (PointLight pos col) = normalize $ pos - location isect
--         l (Dir)
