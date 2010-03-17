-- | Contains the code for applying local lightning. It supports `dynamically'
--   adding of more lightning methods provided the information provided
module Renderer.Lightning where
  
-- | Applies a list of color transformations using the int. info, the 
--   available lights (TODO: Where do we do occlusion testing??? Should this
--   also contain soft shadows???) for the moment this assumes that the
--   provided lights are the visible lights. :)
applyLocalLightning :: IntersectionInfo -> [Light] -> Colour -> Colour
applyLocalLightning = undefined

diffuse = undefined
specular = undefined
fresnel = undefined